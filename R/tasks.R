#' The TabArena-v0.1 task suite
#'
#' Returns the curated task table shipped with the package: one row per
#' dataset with its OpenML task id, target column, problem type, official
#' metric and the number of outer cross-validation splits (`n_folds = 3`
#' folds times `n_repeats` repeats; 10 repeats for datasets with fewer than
#' 2500 rows, 3 otherwise).
#'
#' @param subset Optional name of a dataset-level subset, or a character vector
#'   of dataset names / integer OpenML task ids to keep. Named subsets follow
#'   the official leaderboard: `"all"`, `"binary"`, `"multiclass"`,
#'   `"classification"`, `"regression"`, `"tiny"` (at most 2000 training
#'   rows), `"small"` (at most 10000), `"2k-10k"`, `"medium"` (10001 to
#'   100000), `"tabpfn"` (datasets TabPFNv2 can run on), `"tabicl"`,
#'   `"numerical"`, `"low_cats"`, `"high_cats"`, `"low_features"`,
#'   `"high_features"`.
#' @return A data.frame with columns `dataset`, `task_id`, `dataset_id`,
#'   `target`, `problem_type`, `metric`, `n_folds`, `n_repeats`, `n_splits`,
#'   `n_instances`, `n_features`, `n_classes`, `n_train`, `n_test`,
#'   `pct_categorical`, `can_run_tabpfnv2`, `can_run_tabicl`, `domain`,
#'   `source`, `licence`.
#' @examples
#' head(ta_tasks())
#' ta_tasks("regression")$dataset
#' @export
ta_tasks <- function(subset = NULL) {
  path <- system.file("extdata", "tabarena_v0.1_tasks.csv", package = "tabarenaR")
  tasks <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  tasks$can_run_tabpfnv2 <- as.logical(tasks$can_run_tabpfnv2)
  tasks$can_run_tabicl <- as.logical(tasks$can_run_tabicl)
  tasks$n_splits <- tasks$n_folds * tasks$n_repeats
  tasks$n_train <- round(tasks$n_instances * (tasks$n_folds - 1) / tasks$n_folds)
  tasks$n_test <- tasks$n_instances - tasks$n_train
  if (is.null(subset)) return(tasks)
  ta_subset_tasks(tasks, subset)
}

ta_subset_tasks <- function(tasks, subset) {
  if (is.data.frame(subset)) return(tasks[tasks$dataset %in% subset$dataset, , drop = FALSE])
  if (is.numeric(subset)) return(tasks[tasks$task_id %in% subset, , drop = FALSE])
  if (length(subset) > 1 || !subset %in% ta_subset_names()) {
    unknown <- setdiff(subset, tasks$dataset)
    if (length(unknown)) stop("Unknown dataset(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    return(tasks[tasks$dataset %in% subset, , drop = FALSE])
  }
  keep <- switch(subset,
    all = rep(TRUE, nrow(tasks)),
    lite = rep(TRUE, nrow(tasks)),
    binary = tasks$problem_type == "binary",
    multiclass = tasks$problem_type == "multiclass",
    classification = tasks$problem_type %in% c("binary", "multiclass"),
    regression = tasks$problem_type == "regression",
    tiny = tasks$n_train <= 2000,
    small = tasks$n_train <= 10000,
    `2k-10k` = tasks$n_train > 2000 & tasks$n_train <= 10000,
    medium = tasks$n_train > 10000 & tasks$n_train <= 100000,
    tabpfn = tasks$n_train <= 10000 & tasks$n_features <= 500 & (tasks$problem_type == "regression" | tasks$n_classes <= 10),
    tabicl = tasks$n_train <= 100000 & tasks$n_features <= 500 & tasks$problem_type != "regression",
    numerical = tasks$pct_categorical == 0,
    low_cats = tasks$pct_categorical <= 50,
    high_cats = tasks$pct_categorical > 50,
    low_features = tasks$n_features <= 500,
    high_features = tasks$n_features > 500
  )
  tasks[keep, , drop = FALSE]
}

ta_subset_names <- function() {
  c("all", "lite", "binary", "multiclass", "classification", "regression", "tiny", "small",
    "2k-10k", "medium", "tabpfn", "tabicl", "numerical", "low_cats", "high_cats",
    "low_features", "high_features")
}

#' The (dataset, split) grid of the benchmark
#'
#' Expands the task table into one row per outer split. The `split` index is
#' what the official results files call `fold`: `split = n_folds * repeat +
#' fold` (0-based).
#'
#' @param tasks A task table from [ta_tasks()].
#' @param splits Which splits to keep: `"all"`, `"lite"` (split 0 only, i.e.
#'   TabArena-Lite), or an integer vector of split indices.
#' @return A data.frame with columns `dataset`, `task_id`, `split`, `repeat`,
#'   `fold`, `problem_type`, `metric`, `n_train`, `n_test`.
#' @export
ta_task_grid <- function(tasks = ta_tasks(), splits = "all") {
  rows <- lapply(seq_len(nrow(tasks)), function(i) {
    t <- tasks[i, ]
    s <- seq_len(t$n_splits) - 1L
    data.frame(
      dataset = t$dataset, task_id = t$task_id, split = s,
      `repeat` = s %/% t$n_folds, fold = s %% t$n_folds,
      problem_type = t$problem_type, metric = t$metric,
      n_train = t$n_train, n_test = t$n_test,
      stringsAsFactors = FALSE, check.names = FALSE
    )
  })
  grid <- do.call(rbind, rows)
  rownames(grid) <- NULL
  keep <- ta_resolve_splits(grid$split, splits)
  grid[keep, , drop = FALSE]
}

# Returns a logical vector selecting which of `split_idx` are requested.
ta_resolve_splits <- function(split_idx, splits) {
  if (is.character(splits)) {
    splits <- match.arg(splits, c("all", "lite", "first_repeat"))
    return(switch(splits,
      all = rep(TRUE, length(split_idx)),
      lite = split_idx == 0L,
      first_repeat = split_idx < 3L
    ))
  }
  split_idx %in% as.integer(splits)
}
