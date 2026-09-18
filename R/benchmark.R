#' Evaluate a learner on TabArena and get its Elo in one call
#'
#' Runs [ta_evaluate()] and then [ta_leaderboard()].
#'
#' @inheritParams ta_evaluate
#' @inheritParams ta_leaderboard
#' @param bootstrap_rounds Number of dataset-level bootstrap rounds for the
#'   Elo confidence interval (see [ta_elo()]).
#' @param seed Seed used both for the learner (see [ta_evaluate()]) and the
#'   bootstrap.
#' @return A list with `results` (the per-split results), `leaderboard` (the
#'   full leaderboard) and `summary` (the new method's leaderboard row).
#' @examples
#' \dontrun{
#' lm_learner <- ta_learner(
#'   name = "lm / glm (R)",
#'   train = function(X, y, ctx) {
#'     d <- cbind(X, .y = y)
#'     if (ctx$problem_type == "regression") lm(.y ~ ., d)
#'     else if (ctx$problem_type == "binary") glm(.y ~ ., d, family = binomial())
#'     else nnet::multinom(.y ~ ., d, trace = FALSE)
#'   },
#'   predict = function(model, X, ctx) {
#'     if (ctx$problem_type == "regression") predict(model, X)
#'     else if (ctx$problem_type == "binary") predict(model, X, type = "response")
#'     else predict(model, X, type = "probs")
#'   },
#'   preprocess = function(X_train, X_test, ctx) ta_prep_basic(X_train, X_test, one_hot = TRUE)
#' )
#' out <- ta_benchmark(lm_learner, tasks = ta_tasks("tiny"), splits = "lite")
#' out$summary
#' }
#' @export
ta_benchmark <- function(learner, tasks = ta_tasks(), splits = "lite", reference = "paper",
                         imputation = FALSE, calibration = "RF (default)", bootstrap_rounds = 100,
                         seed = 0, include_systems = TRUE, results_dir = NULL, stop_on_error = FALSE) {
  results <- ta_evaluate(learner, tasks = tasks, splits = splits, results_dir = results_dir,
                         stop_on_error = stop_on_error, seed = seed)
  lb <- ta_leaderboard(results, reference = reference, tasks = tasks, splits = "evaluated",
                       imputation = imputation, calibration = calibration,
                       bootstrap_rounds = bootstrap_rounds, seed = seed,
                       include_systems = include_systems)
  summary <- as.data.frame(lb)[lb$is_new, , drop = FALSE]
  ta_msg(sprintf("%s: Elo %.0f (+%.0f/-%.0f), position %d of %d on %d datasets / %d splits.",
                 learner$name, summary$elo[1], summary$elo_plus[1], summary$elo_minus[1],
                 summary$position[1], nrow(lb), summary$n_datasets[1], summary$n_splits[1]))
  list(results = results, leaderboard = lb, summary = summary)
}
