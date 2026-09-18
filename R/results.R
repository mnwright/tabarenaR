#' Published per-task results of the TabArena leaderboard methods
#'
#' @param source `"paper"` returns the snapshot bundled with the package: the
#'   results of the 45 methods of the TabArena paper leaderboard (default,
#'   tuned and tuned + ensembled variants of 16 models, as published in the
#'   `TabArena/benchmark_results` dataset on Hugging Face, plus AutoGluon 1.3
#'   from TabArena's public store) on all 816 (dataset, split) tasks. `"live"`
#'   downloads the current per-method result files that the online
#'   leaderboard is generated from (see [ta_results_live()]).
#' @param ... Passed on to [ta_results_live()].
#' @return A data.frame with columns `method`, `dataset`, `fold` (split
#'   index), `metric_error`, `time_train_s`, `time_infer_s`, `metric`,
#'   `problem_type`, `metric_error_val`, `method_type`, `method_class`,
#'   `ta_name`, `ta_suite`.
#' @export
ta_results <- function(source = c("paper", "live"), ...) {
  source <- match.arg(source)
  switch(source, paper = ta_results_paper(), live = ta_results_live(...))
}

#' @rdname ta_results
#' @export
ta_results_paper <- function() {
  path <- system.file("extdata", "tabarena_paper_results.rds", package = "tabarenaR")
  df <- readRDS(path)
  df$method_class <- ifelse(grepl("^AutoGluon", df$method), "system", "model")
  df$method_type <- ifelse(df$method_class == "system", "baseline", "config")
  df$ta_name <- df$method
  df$ta_suite <- "tabarena-2025-06-12"
  df
}

ta_results_columns <- function() {
  c("method", "dataset", "fold", "metric_error", "time_train_s", "time_infer_s", "metric",
    "problem_type", "metric_error_val", "method_type", "method_class", "ta_name", "ta_suite")
}

#' Download the live per-method results behind the online leaderboard
#'
#' TabArena publishes one result file per benchmarked method (in a public
#' Cloudflare R2 bucket or the legacy public S3 bucket). This function reads
#' the method registry (see [ta_method_registry()]), downloads every method's
#' results file, and combines them.
#'
#' @param registry A registry data.frame from [ta_method_registry()].
#' @param current_only Keep only the methods on the current leaderboard
#'   (`current == TRUE` in the registry), dropping superseded runs.
#' @param use_display_names Rename the methods as the website does, i.e.
#'   `"<display name> (default|tuned|tuned + ensemble)"` (for example
#'   `"RandomForest (default)"` instead of the internal `"RF (default)"`).
#'   The original identifier is kept in column `method_id`.
#' @param refresh Re-download files already in the cache.
#' @param verbose Print progress.
#' @return A results data.frame (see [ta_results()]) with the extra columns
#'   `method_id`, `display_name`, `verified` and `commercial_use`.
#' @export
ta_results_live <- function(registry = ta_method_registry(), current_only = TRUE,
                            use_display_names = TRUE, refresh = FALSE, verbose = TRUE) {
  if (current_only) registry <- registry[registry$current %in% TRUE, , drop = FALSE]
  out <- vector("list", nrow(registry))
  for (i in seq_len(nrow(registry))) {
    r <- registry[i, ]
    df <- tryCatch(
      ta_results_method(r$method, r$suite, method_type = r$method_type, cache_type = r$cache_type,
                        refresh = refresh),
      error = function(e) {
        warning("Could not load results for ", r$method, " (", r$suite, "): ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
    if (is.null(df)) next
    df$method_class <- if (!is.na(r$method_class)) r$method_class else "model"
    df$method_id <- df$method
    df$display_name <- if (!is.na(r$display_name)) r$display_name else r$method
    df$verified <- r$verified
    df$commercial_use <- r$commercial_use
    if (use_display_names && r$method_class != "system") {
      variant <- regmatches(df$method, regexpr("\\([^()]*\\)$", df$method))
      has_variant <- grepl("\\([^()]*\\)$", df$method)
      df$method[has_variant] <- paste(df$display_name[has_variant], variant)
    }
    if (verbose) ta_msg(sprintf("[%d/%d] %s: %d rows, %d method variant(s)", i, nrow(registry),
                                r$method, nrow(df), length(unique(df$method))))
    out[[i]] <- df
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# Method names that denote the default random forest across naming schemes.
ta_calibration_aliases <- function(calibration) {
  if (is.null(calibration)) return(NULL)
  if (calibration %in% c("RF (default)", "RandomForest (default)")) {
    return(c(calibration, setdiff(c("RF (default)", "RandomForest (default)"), calibration)))
  }
  calibration
}

ta_result_bases <- function(cache_type = NA) {
  r2 <- "https://data.tabarena.ai/"
  s3 <- "https://tabarena.s3.us-west-2.amazonaws.com/"
  if (identical(cache_type, "s3")) c(s3, r2) else c(r2, s3)
}

ta_results_method <- function(method, suite, method_type = "config", cache_type = NA, refresh = FALSE) {
  file <- switch(if (is.na(method_type)) "config" else method_type,
                 config = "hpo_results.parquet", baseline = "model_results.parquet",
                 portfolio = "portfolio_results.parquet", "model_results.parquet")
  dest <- file.path(ta_cache_dir(), "results", suite, method, file)
  if (refresh && file.exists(dest)) unlink(dest)
  if (!file.exists(dest)) {
    key <- paste0("cache/artifacts/", suite, "/methods/", utils::URLencode(method), "/results/", file)
    ok <- FALSE
    for (base in ta_result_bases(cache_type)) {
      ok <- ta_download(paste0(base, key), dest, retries = 2L, must_exist = FALSE)
      if (ok) break
    }
    if (!ok) stop("results file not found on R2 or S3 (", key, ")", call. = FALSE)
  }
  df <- ta_read_results_parquet(dest)
  ta_tidy_result_file(df, method = method, suite = suite, method_type = method_type)
}

# Top-level (non-nested) column names of a parquet file, in file order.
ta_parquet_root_columns <- function(path) {
  s <- nanoparquet::read_parquet_schema(path)
  n <- nrow(s)
  walk <- function(pos) {
    nc <- s$num_children[pos]
    pos <- pos + 1L
    if (!is.na(nc) && nc > 0) for (k in seq_len(nc)) pos <- walk(pos)
    pos
  }
  root_children <- s$num_children[1]
  out <- character()
  pos <- 2L
  for (k in seq_len(if (is.na(root_children)) 0L else root_children)) {
    if (pos > n) break
    if (is.na(s$num_children[pos]) || s$num_children[pos] == 0) out <- c(out, s$name[pos])
    pos <- walk(pos)
  }
  # nanoparquet selects by name, so names that also occur inside a nested
  # column are ambiguous and are left out.
  counts <- table(s$name)
  out[counts[out] == 1]
}

# Reads the flat result columns of a hosted results file. Some files carry a
# nested `method_metadata` column that nanoparquet cannot read at all (even
# when it is not selected); those fall back to the 'arrow' package.
ta_read_results_parquet <- function(path) {
  wanted <- c(ta_results_columns(), "framework", "method_subtype", "config_type", "imputed", "impute_method")
  schema <- nanoparquet::read_parquet_schema(path)
  nested <- any(!is.na(schema$num_children[-1]) & schema$num_children[-1] > 0)
  cols <- unique(intersect(ta_parquet_root_columns(path), wanted))
  if (!nested) return(as.data.frame(nanoparquet::read_parquet(path, col_select = cols)))
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("this results file has a nested column that nanoparquet cannot read; ",
         "install the 'arrow' package (install.packages(\"arrow\")) to read it", call. = FALSE)
  }
  df <- arrow::read_parquet(path, col_select = tidyselect_all_of(cols), as_data_frame = TRUE)
  as.data.frame(df)
}

# arrow's col_select uses tidyselect semantics; a character vector is
# interpreted as column names, which is what we need.
tidyselect_all_of <- function(cols) cols

ta_tidy_result_file <- function(df, method, suite, method_type) {
  if (!"method" %in% names(df) && "framework" %in% names(df)) df$method <- df$framework
  if (!"ta_name" %in% names(df)) df$ta_name <- method
  if (!"ta_suite" %in% names(df)) df$ta_suite <- suite
  if (!"method_type" %in% names(df)) df$method_type <- method_type
  if (!"metric_error_val" %in% names(df)) df$metric_error_val <- NA_real_
  if ("imputed" %in% names(df)) {
    imp <- df$imputed
    imp[is.na(imp)] <- FALSE
    df <- df[!as.logical(imp), , drop = FALSE]
  }
  cols <- ta_results_columns()
  for (c in setdiff(cols, names(df))) df[[c]] <- NA
  df <- df[, cols, drop = FALSE]
  df$fold <- as.integer(df$fold)
  df$metric_error <- as.numeric(df$metric_error)
  df$time_train_s <- as.numeric(df$time_train_s)
  df$time_infer_s <- as.numeric(df$time_infer_s)
  df$method <- as.character(df$method)
  df$dataset <- as.character(df$dataset)
  df
}
