#' tabarenaR: TabArena evaluation entirely in R
#'
#' The package reproduces the TabArena benchmark pipeline in R:
#'
#' 1. [ta_tasks()] lists the 51 TabArena-v0.1 tasks (OpenML task ids, problem
#'    type, metric, number of splits).
#' 2. [ta_load_task()] downloads a task's data and its official outer
#'    cross-validation splits from OpenML and caches them locally.
#' 3. [ta_learner()] wraps any pair of R functions `train(X, y, ctx)` and
#'    `predict(model, X, ctx)`.
#' 4. [ta_evaluate()] runs the learner on the requested tasks and splits and
#'    scores it with the official metric of each task (1 - ROC AUC for binary
#'    classification, log loss for multiclass, RMSE for regression).
#' 5. [ta_results()] loads the published per-task results of the leaderboard
#'    methods (a bundled snapshot of the paper results, or the live results
#'    that back the online leaderboard).
#' 6. [ta_leaderboard()] and [ta_elo()] compute the Bradley-Terry Elo with
#'    task-level bootstrap confidence intervals, calibrated so that
#'    `RF (default)` sits at 1000, plus win rate, mean rank, harmonic rank,
#'    normalized score, improvability and timing columns, mirroring the
#'    official implementation.
#' 7. [ta_benchmark()] chains steps 4 to 6 in one call.
#'
#' Downloads are cached under `tools::R_user_dir("tabarenaR", "cache")`;
#' set `options(tabarenaR.cache_dir = "...")` or the environment variable
#' `TABARENA_R_CACHE` to change the location.
#'
#' @references Erickson, N., Purucker, L., Tschalzev, A., Holzmüller, D.,
#'   Desai, P. S., Salinas, D., & Hutter, F. (2025). TabArena: A Living
#'   Benchmark for Machine Learning on Tabular Data.
#'   \url{https://arxiv.org/abs/2506.16791}
#' @keywords internal
"_PACKAGE"
