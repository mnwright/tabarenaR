#' Compute a TabArena leaderboard
#'
#' Scores one or more new methods against the published results of the
#' leaderboard methods and returns the leaderboard columns of the official
#' website: Elo (with 95% interval), normalized score, mean rank, harmonic
#' rank, improvability, win rate, median train / predict time per 1000 rows
#' and the share of imputed tasks.
#'
#' @param results Results of the new method(s) as returned by
#'   [ta_evaluate()] (or any data.frame with `method`, `dataset`, `fold`,
#'   `metric_error`, and ideally `time_train_s`, `time_infer_s`). `NULL`
#'   reproduces the leaderboard of the reference methods alone.
#' @param reference The reference results: `"paper"` (bundled snapshot of the
#'   TabArena paper results, 44 methods), `"live"` (downloads the current
#'   per-method results behind the online leaderboard, see [ta_results()]),
#'   or a results data.frame.
#' @param tasks Which datasets form the field: `NULL` (all 51), a subset name
#'   accepted by [ta_tasks()], a character vector of dataset names, or a task
#'   table.
#' @param splits Which splits form the field: `"evaluated"` (default; the
#'   (dataset, split) pairs the new results cover, so that a TabArena-Lite run
#'   is compared on TabArena-Lite), `"all"`, `"lite"` or integer split
#'   indices.
#' @param imputation If `FALSE` (default; the website's "no imputation"
#'   board) methods lacking a result on any task of the field are dropped. If
#'   `TRUE`, their missing tasks are filled with the result of the
#'   `calibration` method (a default random forest) and flagged as imputed,
#'   like the website's imputation toggle.
#' @param calibration The method pinned to Elo 1000 and used to fill missing
#'   results; `"RF (default)"` on the official leaderboard.
#' @param bootstrap_rounds,seed Bootstrap settings, see [ta_elo()].
#' @param include_systems Keep whole-pipeline systems such as AutoGluon in the
#'   field (`TRUE`), or compare against individual models only (`FALSE`, the
#'   website's default pool). Requires `method_class` information, which the
#'   live results carry.
#' @param methods Optional character vector restricting the reference methods.
#' @param verbose Print progress.
#' @return A data.frame of class `ta_leaderboard`, one row per method sorted
#'   by Elo, with attributes `"per_task"` (the per-(method, dataset, split)
#'   table with rank, improvability etc.), `"elo"` (the [ta_elo()] output),
#'   `"dropped"` (methods removed for missing results) and `"field"` (the
#'   (dataset, split) pairs used).
#' @seealso [ta_benchmark()] to evaluate and rank in one call.
#' @export
ta_leaderboard <- function(results = NULL, reference = "paper", tasks = NULL, splits = "evaluated",
                           imputation = FALSE, calibration = "RF (default)", bootstrap_rounds = 100,
                           seed = 0, include_systems = TRUE, methods = NULL, verbose = TRUE) {
  ref <- if (is.data.frame(reference)) reference else ta_results(source = reference)
  ref <- ta_standardize_results(ref, is_new = FALSE)
  if (!is.null(calibration) && !calibration %in% ref$method) {
    alt <- intersect(ta_calibration_aliases(calibration), unique(ref$method))
    if (length(alt)) calibration <- alt[1]
  }
  if (!is.null(methods)) ref <- ref[ref$method %in% c(methods, calibration), , drop = FALSE]
  if (!include_systems && "method_class" %in% names(ref)) {
    ref <- ref[is.na(ref$method_class) | ref$method_class != "system", , drop = FALSE]
  }
  new <- if (is.null(results)) NULL else ta_standardize_results(results, is_new = TRUE)
  if (!is.null(new)) {
    clash <- intersect(unique(new$method), unique(ref$method))
    if (length(clash)) ref <- ref[!ref$method %in% clash, , drop = FALSE]
  }
  all_res <- rbind(ref, new)

  # --- the field: (dataset, split) pairs ------------------------------------
  task_tab <- ta_tasks()
  if (!is.null(tasks)) task_tab <- ta_subset_tasks(task_tab, tasks)
  if (identical(splits, "evaluated")) {
    if (is.null(new)) {
      grid <- ta_task_grid(task_tab, "all")
    } else {
      done <- unique(new[!is.na(new$metric_error), c("dataset", "fold")])
      grid <- ta_task_grid(task_tab, "all")
      grid <- grid[paste(grid$dataset, grid$split) %in% paste(done$dataset, done$fold), , drop = FALSE]
    }
  } else {
    grid <- ta_task_grid(task_tab, splits)
  }
  if (!nrow(grid)) stop("The field of (dataset, split) pairs is empty.", call. = FALSE)
  field_key <- paste(grid$dataset, grid$split)
  all_res <- all_res[paste(all_res$dataset, all_res$fold) %in% field_key, , drop = FALSE]
  all_res <- all_res[!is.na(all_res$metric_error), , drop = FALSE]
  if (!is.null(calibration)) {
    calib_rows <- all_res[all_res$method == calibration, , drop = FALSE]
    if (!nrow(calib_rows)) stop("Calibration method '", calibration, "' has no results on the field.", call. = FALSE)
    have <- paste(calib_rows$dataset, calib_rows$fold)
    if (!all(field_key %in% have)) {
      grid <- grid[field_key %in% have, , drop = FALSE]
      field_key <- paste(grid$dataset, grid$split)
      all_res <- all_res[paste(all_res$dataset, all_res$fold) %in% field_key, , drop = FALSE]
    }
  }
  all_res <- ta_average_duplicates(all_res)

  # --- imputation / dropping ------------------------------------------------
  per_method <- split(all_res, all_res$method)
  n_units <- nrow(grid)
  dropped <- character()
  filled <- list()
  fill_src <- if (!is.null(calibration)) all_res[all_res$method == calibration, , drop = FALSE] else NULL
  for (m in names(per_method)) {
    d <- per_method[[m]]
    have <- paste(d$dataset, d$fold)
    missing <- setdiff(field_key, have)
    d$imputed <- FALSE
    if (length(missing) == 0) {
      filled[[m]] <- d
      next
    }
    if (!imputation || is.null(fill_src) || length(missing) == n_units) {
      dropped <- c(dropped, m)
      next
    }
    add <- fill_src[paste(fill_src$dataset, fill_src$fold) %in% missing, , drop = FALSE]
    add$method <- m
    add$imputed <- TRUE
    for (col in c("is_new", "method_class", "method_type", "ta_name", "ta_suite")) {
      if (col %in% names(d)) add[[col]] <- d[[col]][1]
    }
    filled[[m]] <- rbind(d, add[, names(d), drop = FALSE])
  }
  if (length(dropped) && verbose) {
    ta_msg(length(dropped), " method(s) dropped for missing results on the field",
           if (!imputation) " (set imputation = TRUE to keep them)" else "", ": ",
           paste(utils::head(dropped, 8), collapse = ", "), if (length(dropped) > 8) ", ..." else "")
  }
  res <- do.call(rbind, filled)
  rownames(res) <- NULL
  if (length(unique(res$method)) < 2) stop("Fewer than two methods remain on the field.", call. = FALSE)

  # --- per-unit metrics -----------------------------------------------------
  res$n_train <- grid$n_train[match(res$dataset, grid$dataset)]
  res$n_test <- grid$n_test[match(res$dataset, grid$dataset)]
  res$time_train_s_per_1K <- res$time_train_s * 1000 / res$n_train
  res$time_infer_s_per_1K <- res$time_infer_s * 1000 / res$n_test
  unit <- paste(res$dataset, res$fold)
  res$rank <- stats::ave(res$metric_error, unit, FUN = function(e) rank(e, ties.method = "average"))
  n_in_unit <- stats::ave(res$metric_error, unit, FUN = length)
  res$winrate <- 1 - (res$rank - 1) / pmax(n_in_unit - 1, 1)
  res$mrr <- 1 / res$rank
  best <- stats::ave(res$metric_error, unit, FUN = min)
  res$improvability <- ifelse(res$metric_error > 0, 1 - best / res$metric_error, 0)

  # normalized error at dataset level (mean error over splits, then
  # (err - best) / (median - best) clipped to [0, 1])
  ds_mean <- stats::aggregate(metric_error ~ method + dataset, data = res, FUN = mean)
  top <- stats::ave(ds_mean$metric_error, ds_mean$dataset, FUN = min)
  med <- stats::ave(ds_mean$metric_error, ds_mean$dataset, FUN = stats::median)
  ds_mean$normalized_error <- pmin(pmax((ds_mean$metric_error - top) / pmax(med - top, 1e-5), 0), 1)
  res$normalized_error <- ds_mean$normalized_error[match(paste(res$method, res$dataset),
                                                         paste(ds_mean$method, ds_mean$dataset))]

  # --- aggregation: mean over splits within dataset, then over datasets -----
  agg_cols <- c("metric_error", "rank", "winrate", "mrr", "improvability", "imputed",
                "time_train_s", "time_infer_s", "time_train_s_per_1K", "time_infer_s_per_1K",
                "normalized_error")
  res$imputed <- as.numeric(res$imputed)
  by_ds <- stats::aggregate(res[agg_cols], by = list(method = res$method, dataset = res$dataset),
                            FUN = mean, na.rm = TRUE)
  by_m <- stats::aggregate(by_ds[agg_cols], by = list(method = by_ds$method), FUN = mean, na.rm = TRUE)
  med_m <- stats::aggregate(by_ds[c("time_train_s_per_1K", "time_infer_s_per_1K")],
                            by = list(method = by_ds$method), FUN = stats::median, na.rm = TRUE)
  names(med_m)[-1] <- paste0("median_", names(med_m)[-1])
  lb <- merge(by_m, med_m, by = "method")

  # --- Elo ------------------------------------------------------------------
  if (verbose) ta_msg("Fitting Elo (", length(unique(res$method)), " methods, ",
                      length(unique(res$dataset)), " datasets, ", n_units, " splits, ",
                      bootstrap_rounds, " bootstrap rounds) ...")
  elo <- ta_elo(res, calibration = calibration, bootstrap_rounds = bootstrap_rounds, seed = seed)
  lb <- merge(lb, elo[, c("method", "elo", "elo_plus", "elo_minus")], by = "method")

  meta <- unique(res[, intersect(c("method", "is_new", "method_class", "ta_suite"), names(res)), drop = FALSE])
  meta <- meta[!duplicated(meta$method), , drop = FALSE]
  lb <- merge(lb, meta, by = "method", all.x = TRUE)
  n_ds <- tapply(res$dataset, res$method, function(x) length(unique(x)))
  lb$n_datasets <- as.integer(n_ds[lb$method])
  lb$n_splits <- n_units

  out <- data.frame(
    method = lb$method,
    elo = lb$elo, elo_plus = lb$elo_plus, elo_minus = lb$elo_minus,
    score = 1 - lb$normalized_error,
    rank = lb$rank,
    harmonic_rank = 1 / lb$mrr,
    improvability_pct = 100 * lb$improvability,
    winrate = lb$winrate,
    median_time_train_s_per_1K = lb$median_time_train_s_per_1K,
    median_time_infer_s_per_1K = lb$median_time_infer_s_per_1K,
    imputed_pct = 100 * lb$imputed,
    n_datasets = lb$n_datasets, n_splits = lb$n_splits,
    is_new = if ("is_new" %in% names(lb)) lb$is_new %in% TRUE else FALSE,
    stringsAsFactors = FALSE
  )
  if ("method_class" %in% names(lb)) out$method_class <- lb$method_class
  out <- out[order(-out$elo), , drop = FALSE]
  out$position <- seq_len(nrow(out))
  out <- out[, c("position", setdiff(names(out), "position"))]
  rownames(out) <- NULL
  attr(out, "per_task") <- res
  attr(out, "elo") <- elo
  attr(out, "dropped") <- dropped
  attr(out, "field") <- grid[, c("dataset", "split")]
  class(out) <- c("ta_leaderboard", "data.frame")
  out
}

#' @export
print.ta_leaderboard <- function(x, n = Inf, digits = 1, ...) {
  df <- as.data.frame(x)
  show <- data.frame(
    `#` = df$position,
    method = paste0(ifelse(df$is_new, "* ", "  "), df$method),
    elo = round(df$elo, 0),
    `95% CI` = sprintf("+%.0f/-%.0f", df$elo_plus, df$elo_minus),
    score = round(df$score, 3),
    rank = round(df$rank, 2),
    `harm. rank` = round(df$harmonic_rank, 2),
    `improv. %` = round(df$improvability_pct, 2),
    winrate = round(df$winrate, 3),
    `train s/1K` = signif(df$median_time_train_s_per_1K, 3),
    `pred s/1K` = signif(df$median_time_infer_s_per_1K, 3),
    `imputed %` = round(df$imputed_pct, 1),
    check.names = FALSE, stringsAsFactors = FALSE
  )
  field <- attr(x, "field")
  cat("TabArena leaderboard: ", nrow(df), " methods on ", length(unique(field$dataset)),
      " datasets / ", nrow(field), " splits", if (any(df$is_new)) "  (* = your method)" else "", "\n", sep = "")
  print(utils::head(show, n), row.names = FALSE, right = FALSE)
  dropped <- attr(x, "dropped")
  if (length(dropped)) cat("(", length(dropped), " methods without complete results were dropped)\n", sep = "")
  invisible(x)
}

# Brings any results frame to the common column set.
ta_standardize_results <- function(df, is_new = FALSE) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  if ("framework" %in% names(df) && !"method" %in% names(df)) df$method <- df$framework
  if ("split" %in% names(df) && !"fold" %in% names(df)) df$fold <- df$split
  need <- c("method", "dataset", "fold", "metric_error")
  miss <- setdiff(need, names(df))
  if (length(miss)) stop("Results are missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  for (col in c("time_train_s", "time_infer_s")) if (!col %in% names(df)) df[[col]] <- NA_real_
  if (!"problem_type" %in% names(df)) df$problem_type <- NA_character_
  if (!"metric" %in% names(df)) df$metric <- NA_character_
  if (!"method_class" %in% names(df)) df$method_class <- NA_character_
  if (!"method_type" %in% names(df)) df$method_type <- NA_character_
  if (!"ta_suite" %in% names(df)) df$ta_suite <- NA_character_
  if (!"method_id" %in% names(df)) df$method_id <- df$method
  out <- data.frame(
    method = as.character(df$method), dataset = as.character(df$dataset), fold = as.integer(df$fold),
    metric_error = as.numeric(df$metric_error), time_train_s = as.numeric(df$time_train_s),
    time_infer_s = as.numeric(df$time_infer_s), problem_type = as.character(df$problem_type),
    metric = as.character(df$metric), method_class = as.character(df$method_class),
    method_type = as.character(df$method_type), ta_suite = as.character(df$ta_suite),
    method_id = as.character(df$method_id), is_new = is_new, stringsAsFactors = FALSE
  )
  out
}

ta_average_duplicates <- function(df) {
  key <- paste(df$method, df$dataset, df$fold, sep = "\r")
  if (!anyDuplicated(key)) return(df)
  warning("Duplicate (method, dataset, split) rows were averaged.", call. = FALSE)
  num <- c("metric_error", "time_train_s", "time_infer_s")
  agg <- stats::aggregate(df[num], by = list(key = key), FUN = mean, na.rm = TRUE)
  first <- df[!duplicated(key), , drop = FALSE]
  first[num] <- agg[match(key[!duplicated(key)], agg$key), num]
  first
}
