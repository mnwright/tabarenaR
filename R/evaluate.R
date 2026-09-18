#' Run a learner on TabArena tasks
#'
#' Evaluates a [ta_learner()] on the requested tasks and outer splits with the
#' official protocol: for every split the learner is trained on the training
#' rows, predicts the test rows, and the prediction is scored with the task's
#' metric. Training and prediction wall-clock times are recorded.
#'
#' @param learner A [ta_learner()].
#' @param tasks A task table from [ta_tasks()] (possibly subset).
#' @param splits `"lite"` (split 0 of every dataset, i.e. TabArena-Lite),
#'   `"all"` (every split; 9 or 30 per dataset), `"first_repeat"` (splits 0 to
#'   2) or an integer vector of split indices.
#' @param results_dir Optional directory where every finished (dataset, split)
#'   result is checkpointed as an `.rds` file, so an interrupted run can be
#'   resumed by calling the function again.
#' @param stop_on_error If `FALSE` (default), a failing split records `NA`
#'   as its error (and a warning) instead of aborting the whole run. Such
#'   tasks count as missing / imputed on the leaderboard.
#' @param seed Seed set before every split for reproducibility of the learner.
#' @return A data.frame of per-split results with the columns used by the
#'   official results files: `method`, `dataset`, `fold` (the split index),
#'   `metric_error`, `time_train_s`, `time_infer_s`, `metric`,
#'   `problem_type`, plus `task_id`, `repeat`, `fold_in_repeat` and `error_message`.
#' @export
ta_evaluate <- function(learner, tasks = ta_tasks(), splits = "lite", results_dir = NULL,
                        stop_on_error = FALSE, seed = 0L) {
  stopifnot(inherits(learner, "ta_learner"))
  if (!is.null(results_dir)) dir.create(results_dir, recursive = TRUE, showWarnings = FALSE)
  grid <- ta_task_grid(tasks, splits)
  if (!nrow(grid)) stop("No (task, split) pairs selected.", call. = FALSE)
  ta_msg("Evaluating '", learner$name, "' on ", length(unique(grid$dataset)), " datasets / ",
         nrow(grid), " splits.")
  out <- vector("list", nrow(grid))
  datasets <- unique(grid$dataset)
  k <- 0L
  t_start <- Sys.time()
  for (ds in datasets) {
    g <- grid[grid$dataset == ds, , drop = FALSE]
    task <- NULL
    for (i in seq_len(nrow(g))) {
      k <- k + 1L
      split <- g$split[i]
      ckpt <- if (!is.null(results_dir)) {
        file.path(results_dir, paste0(ta_safe_name(learner$name), "__", ta_safe_name(ds), "__", split, ".rds"))
      } else NULL
      if (!is.null(ckpt) && file.exists(ckpt)) {
        out[[k]] <- readRDS(ckpt)
        next
      }
      if (is.null(task)) task <- ta_load_task(ds, tasks = tasks)
      res <- ta_evaluate_split(learner, task, split, stop_on_error = stop_on_error, seed = seed)
      ta_msg(sprintf("[%d/%d] %s split %d: %s = %s (train %.1fs, predict %.1fs)",
                     k, nrow(grid), ds, split, task$metric,
                     if (is.na(res$metric_error)) "FAILED" else formatC(res$metric_error, digits = 5, format = "g"),
                     res$time_train_s, res$time_infer_s))
      out[[k]] <- res
      if (!is.null(ckpt)) saveRDS(res, ckpt)
    }
  }
  results <- do.call(rbind, out)
  rownames(results) <- NULL
  elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
  ta_msg(sprintf("Done: %d splits in %.1f s; %d failed.", nrow(results), elapsed, sum(is.na(results$metric_error))))
  results
}

#' Evaluate a learner on a single split
#'
#' @inheritParams ta_evaluate
#' @param task A `ta_task` from [ta_load_task()].
#' @param split The 0-based split index.
#' @return A one-row data.frame (see [ta_evaluate()]).
#' @export
ta_evaluate_split <- function(learner, task, split = 0L, stop_on_error = FALSE, seed = 0L) {
  s <- ta_split(task, split)
  ctx <- list(
    dataset = task$dataset, task_id = task$task_id, split = s$split, `repeat` = s$`repeat`,
    fold = s$fold, problem_type = task$problem_type, metric = task$metric, target = task$target,
    levels = task$levels, positive_class = task$positive_class,
    n_classes = if (is.null(task$levels)) NA_integer_ else length(task$levels)
  )
  X_train <- s$X_train
  X_test <- s$X_test
  run <- function() {
    if (!is.null(learner$preprocess)) {
      pp <- learner$preprocess(X_train, X_test, ctx)
      X_train <- pp$X_train
      X_test <- pp$X_test
    }
    if (!is.null(seed)) set.seed(seed + split)
    t0 <- proc.time()[["elapsed"]]
    model <- learner$train(X_train, s$y_train, ctx)
    t1 <- proc.time()[["elapsed"]]
    pred <- learner$predict(model, X_test, ctx)
    t2 <- proc.time()[["elapsed"]]
    err <- ta_metric_error(s$y_test, pred, task$problem_type, levels = task$levels, metric = task$metric)
    list(err = err, time_train = t1 - t0, time_infer = t2 - t1, msg = NA_character_)
  }
  r <- if (stop_on_error) run() else tryCatch(run(), error = function(e) {
    warning("Learner failed on ", task$dataset, " split ", split, ": ", conditionMessage(e), call. = FALSE)
    list(err = NA_real_, time_train = NA_real_, time_infer = NA_real_, msg = conditionMessage(e))
  })
  data.frame(
    method = learner$name, dataset = task$dataset, fold = as.integer(split),
    metric_error = as.numeric(r$err), time_train_s = as.numeric(r$time_train),
    time_infer_s = as.numeric(r$time_infer), metric = task$metric,
    problem_type = task$problem_type, task_id = task$task_id,
    `repeat` = s$`repeat`, fold_in_repeat = s$fold, error_message = r$msg,
    stringsAsFactors = FALSE, check.names = FALSE
  )
}

ta_safe_name <- function(x) gsub("[^A-Za-z0-9_.-]+", "_", x)

#' Collect checkpointed results
#'
#' Reads every `.rds` checkpoint written by [ta_evaluate()] to `results_dir`
#' and returns them as one results data.frame.
#'
#' @param results_dir The checkpoint directory.
#' @return A results data.frame.
#' @export
ta_collect_results <- function(results_dir) {
  files <- list.files(results_dir, pattern = "\\.rds$", full.names = TRUE)
  if (!length(files)) return(NULL)
  res <- do.call(rbind, lapply(files, readRDS))
  rownames(res) <- NULL
  res
}
