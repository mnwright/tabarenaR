ta_openml_base <- function() getOption("tabarenaR.openml_url", "https://www.openml.org")

ta_openml_json <- function(endpoint) {
  url <- paste0(ta_openml_base(), "/api/v1/json/", endpoint)
  cache_file <- file.path(ta_cache_dir(), "openml", "api", paste0(gsub("/", "_", endpoint), ".json"))
  ta_read_json_url(url, cache_file)
}

# Task description: dataset id, target, estimation procedure, split file url.
ta_openml_task_meta <- function(task_id) {
  js <- ta_openml_json(paste0("task/", task_id))$task
  inputs <- js$input
  source_data <- inputs$data_set[!is.na(inputs$name) & inputs$name == "source_data", ]
  est <- inputs$estimation_procedure[!is.na(inputs$name) & inputs$name == "estimation_procedure", ]
  params <- est$parameter[[1]]
  get_param <- function(name) {
    v <- params$value[params$name == name]
    if (length(v) == 0 || is.na(v)) NA_integer_ else as.integer(v)
  }
  list(
    task_id = as.integer(js$task_id),
    task_type = js$task_type,
    dataset_id = as.integer(source_data$data_set_id),
    target = source_data$target_feature,
    n_repeats = get_param("number_repeats"),
    n_folds = get_param("number_folds"),
    splits_url = est$data_splits_url
  )
}

ta_openml_dataset_meta <- function(dataset_id) {
  ta_openml_json(paste0("data/", dataset_id))$data_set_description
}

ta_openml_features <- function(dataset_id) {
  f <- ta_openml_json(paste0("data/features/", dataset_id))$data_features$feature
  f$index <- as.integer(f$index)
  f$is_target <- as.logical(f$is_target)
  f$is_ignore <- as.logical(f$is_ignore)
  f$is_row_identifier <- as.logical(f$is_row_identifier)
  f[order(f$index), , drop = FALSE]
}

# Reads the raw table of an OpenML dataset (parquet preferred, ARFF fallback).
ta_openml_read_raw <- function(dataset_id, meta) {
  dir <- file.path(ta_cache_dir(), "openml", "datasets", dataset_id)
  pq_url <- meta$parquet_url
  if (!is.null(pq_url) && !is.na(pq_url) && nzchar(pq_url)) {
    pq <- file.path(dir, paste0("dataset_", dataset_id, ".pq"))
    ok <- file.exists(pq) || ta_download(pq_url, pq, must_exist = FALSE)
    if (ok) {
      df <- tryCatch(as.data.frame(nanoparquet::read_parquet(pq)), error = function(e) NULL)
      if (!is.null(df)) return(df)
      ta_msg("Could not read the parquet file of dataset ", dataset_id, ", falling back to ARFF.")
    }
  }
  arff <- file.path(dir, paste0("dataset_", dataset_id, ".arff"))
  if (!file.exists(arff)) ta_download(meta$url, arff)
  if (!requireNamespace("foreign", quietly = TRUE)) {
    stop("Reading ARFF files needs the 'foreign' package.", call. = FALSE)
  }
  foreign::read.arff(arff)
}

# Applies the OpenML feature types to a raw table: nominal -> factor with the
# declared levels, numeric -> numeric; drops ignored / row-id columns.
ta_openml_typed_data <- function(raw, features, target) {
  keep <- features[!features$is_ignore & !features$is_row_identifier, , drop = FALSE]
  missing_cols <- setdiff(keep$name, names(raw))
  if (length(missing_cols)) {
    stop("Dataset is missing declared columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  out <- raw[, keep$name, drop = FALSE]
  for (i in seq_len(nrow(keep))) {
    nm <- keep$name[i]
    type <- keep$data_type[i]
    x <- out[[nm]]
    if (type == "nominal") {
      lv <- keep$nominal_value[[i]]
      chr <- as.character(x)
      if (is.null(lv) || all(is.na(lv))) lv <- sort(unique(chr[!is.na(chr)]))
      extra <- setdiff(unique(chr[!is.na(chr)]), lv)
      if (length(extra)) lv <- c(lv, sort(extra))
      out[[nm]] <- factor(chr, levels = lv)
    } else if (type == "numeric") {
      out[[nm]] <- as.numeric(x)
    } else if (type == "string") {
      out[[nm]] <- as.character(x)
    } else if (type == "date") {
      out[[nm]] <- as.character(x)
    }
  }
  out
}

# Loads the split file of a task and returns the 1-based test indices of every
# split (a list indexed by split + 1).
ta_openml_splits <- function(task_id, splits_url, n_folds, n_repeats) {
  dir <- file.path(ta_cache_dir(), "openml", "splits")
  rds <- file.path(dir, paste0("task_", task_id, "_splits.rds"))
  if (file.exists(rds)) return(readRDS(rds))
  arff <- file.path(dir, paste0("task_", task_id, "_splits.arff"))
  if (!file.exists(arff)) ta_download(splits_url, arff)
  header <- readLines(arff, n = 100L, warn = FALSE)
  data_line <- which(tolower(trimws(header)) == "@data")
  if (!length(data_line)) stop("Malformed split file for task ", task_id, call. = FALSE)
  tab <- utils::read.csv(
    arff, skip = data_line[1], header = FALSE,
    col.names = c("type", "rowid", "rep", "fold"),
    colClasses = c("character", "integer", "integer", "integer"),
    strip.white = TRUE
  )
  tab <- tab[toupper(tab$type) == "TEST", , drop = FALSE]
  split_idx <- n_folds * tab$rep + tab$fold
  n_splits <- n_folds * n_repeats
  test <- split(tab$rowid + 1L, factor(split_idx, levels = seq_len(n_splits) - 1L))
  test <- lapply(test, function(x) sort(as.integer(x)))
  names(test) <- NULL
  out <- list(n_folds = n_folds, n_repeats = n_repeats, test = test)
  saveRDS(out, rds)
  unlink(arff)
  out
}

#' Download and load a TabArena task
#'
#' Fetches the dataset (parquet, with an ARFF fallback) and the official outer
#' cross-validation split indices of a TabArena task from OpenML and caches
#' both locally.
#'
#' @param task A dataset name (e.g. `"credit-g"`), an OpenML task id, or a
#'   one-row slice of [ta_tasks()].
#' @param tasks The task table to resolve names against.
#' @return An object of class `ta_task`: a list with `dataset`, `task_id`,
#'   `dataset_id`, `problem_type`, `metric`, `target`, `levels` (class labels
#'   for classification), `positive_class` (binary), `data` (a data.frame with
#'   the features and the target column), `n_folds`, `n_repeats`, `n_splits`
#'   and `test_idx` (list of 1-based test row indices per split).
#' @examples
#' \dontrun{
#' task <- ta_load_task("credit-g")
#' str(task$data)
#' s <- ta_split(task, 0)
#' dim(s$X_train); dim(s$X_test)
#' }
#' @export
ta_load_task <- function(task, tasks = ta_tasks()) {
  row <- ta_resolve_task_row(task, tasks)
  rds <- file.path(ta_cache_dir(), "openml", "tasks", paste0("task_", row$task_id, ".rds"))
  if (file.exists(rds)) {
    obj <- readRDS(rds)
    return(obj)
  }
  ta_msg("Downloading task ", row$task_id, " (", row$dataset, ") from OpenML ...")
  meta <- ta_openml_task_meta(row$task_id)
  if (!identical(meta$dataset_id, as.integer(row$dataset_id))) {
    warning("OpenML task ", row$task_id, " points to dataset ", meta$dataset_id,
            " but the suite lists ", row$dataset_id, ".", call. = FALSE)
  }
  target <- if (!is.null(meta$target) && nzchar(meta$target)) meta$target else row$target
  dmeta <- ta_openml_dataset_meta(meta$dataset_id)
  features <- ta_openml_features(meta$dataset_id)
  raw <- ta_openml_read_raw(meta$dataset_id, dmeta)
  data <- ta_openml_typed_data(raw, features, target)
  if (!target %in% names(data)) stop("Target column ", target, " not found.", call. = FALSE)
  # Put the target last.
  data <- data[, c(setdiff(names(data), target), target), drop = FALSE]
  problem_type <- row$problem_type
  levels <- NULL
  positive_class <- NULL
  if (problem_type %in% c("binary", "multiclass")) {
    y <- data[[target]]
    if (!is.factor(y)) y <- factor(y)
    present <- levels(y)[levels(y) %in% unique(as.character(y))]
    y <- factor(as.character(y), levels = present)
    data[[target]] <- y
    levels <- present
    if (problem_type == "binary") {
      if (length(levels) != 2) stop("Binary task with ", length(levels), " classes.", call. = FALSE)
      positive_class <- levels[2]
    }
  } else {
    data[[target]] <- as.numeric(data[[target]])
  }
  n_folds <- if (is.na(meta$n_folds)) row$n_folds else meta$n_folds
  n_repeats <- if (is.na(meta$n_repeats)) row$n_repeats else meta$n_repeats
  if (n_folds != row$n_folds) {
    warning("OpenML reports ", n_folds, " folds for ", row$dataset, "; the suite lists ", row$n_folds, ".", call. = FALSE)
  }
  splits <- ta_openml_splits(row$task_id, meta$splits_url, n_folds, n_repeats)
  # TabArena uses only the first `n_repeats` repeats listed in the suite.
  n_use <- row$n_folds * row$n_repeats
  test_idx <- splits$test[seq_len(min(n_use, length(splits$test)))]
  obj <- structure(list(
    dataset = row$dataset, task_id = row$task_id, dataset_id = meta$dataset_id,
    problem_type = problem_type, metric = row$metric, target = target,
    levels = levels, positive_class = positive_class, data = data,
    n_folds = row$n_folds, n_repeats = row$n_repeats, n_splits = length(test_idx),
    test_idx = test_idx
  ), class = "ta_task")
  dir.create(dirname(rds), recursive = TRUE, showWarnings = FALSE)
  saveRDS(obj, rds)
  obj
}

ta_resolve_task_row <- function(task, tasks) {
  if (inherits(task, "ta_task")) task <- task$dataset
  if (is.data.frame(task)) {
    if (nrow(task) != 1) stop("`task` must be a single task.", call. = FALSE)
    return(task)
  }
  if (is.numeric(task)) {
    row <- tasks[tasks$task_id == task, , drop = FALSE]
  } else {
    row <- tasks[tasks$dataset == task, , drop = FALSE]
  }
  if (nrow(row) != 1) stop("Unknown TabArena task: ", task, call. = FALSE)
  row
}

#' @export
print.ta_task <- function(x, ...) {
  cat("<TabArena task> ", x$dataset, " (OpenML task ", x$task_id, ")\n", sep = "")
  cat("  problem type: ", x$problem_type, " | metric: ", x$metric, " | target: ", x$target, "\n", sep = "")
  cat("  rows: ", nrow(x$data), " | features: ", ncol(x$data) - 1L,
      if (!is.null(x$levels)) paste0(" | classes: ", length(x$levels)) else "", "\n", sep = "")
  cat("  splits: ", x$n_splits, " (", x$n_folds, " folds x ", x$n_repeats, " repeats)\n", sep = "")
  invisible(x)
}

#' Train/test data of one outer split
#'
#' @param task A `ta_task` from [ta_load_task()].
#' @param split The 0-based split index (`split = n_folds * repeat + fold`),
#'   between 0 and `task$n_splits - 1`.
#' @return A list with `X_train`, `y_train`, `X_test`, `y_test`, the combined
#'   data.frames `train` and `test` (target column included), and the split
#'   bookkeeping `split`, `repeat`, `fold`, `train_idx`, `test_idx`.
#' @export
ta_split <- function(task, split = 0L) {
  split <- as.integer(split)
  if (split < 0 || split >= task$n_splits) {
    stop("Split must be between 0 and ", task$n_splits - 1L, call. = FALSE)
  }
  test_idx <- task$test_idx[[split + 1L]]
  n <- nrow(task$data)
  train_idx <- setdiff(seq_len(n), test_idx)
  train <- task$data[train_idx, , drop = FALSE]
  test <- task$data[test_idx, , drop = FALSE]
  rownames(train) <- NULL
  rownames(test) <- NULL
  feats <- setdiff(names(task$data), task$target)
  list(
    X_train = train[, feats, drop = FALSE], y_train = train[[task$target]],
    X_test = test[, feats, drop = FALSE], y_test = test[[task$target]],
    train = train, test = test,
    split = split, `repeat` = split %/% task$n_folds, fold = split %% task$n_folds,
    train_idx = train_idx, test_idx = test_idx
  )
}
