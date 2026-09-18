# Network tests: run with TABARENA_R_LIVE_TESTS=true
skip_if_offline_tests <- function() {
  testthat::skip_if_not(identical(Sys.getenv("TABARENA_R_LIVE_TESTS"), "true"),
                        "set TABARENA_R_LIVE_TESTS=true to run network tests")
}

test_that("a small task can be downloaded from OpenML and split", {
  skip_if_offline_tests()
  task <- ta_load_task("credit-g")
  expect_s3_class(task, "ta_task")
  expect_equal(nrow(task$data), 1000)
  expect_equal(task$n_splits, 30)
  expect_equal(task$problem_type, "binary")
  expect_equal(length(task$levels), 2)
  s <- ta_split(task, 0)
  expect_equal(nrow(s$train) + nrow(s$test), 1000)
  expect_length(intersect(s$train_idx, s$test_idx), 0)
  # every row is a test row exactly once per repeat
  test_counts <- table(unlist(task$test_idx[1:3]))
  expect_true(all(test_counts == 1))
  expect_equal(length(test_counts), 1000)
})

test_that("a learner can be evaluated on one split", {
  skip_if_offline_tests()
  lrn <- ta_learner(
    name = "glm",
    train = function(X, y, ctx) suppressWarnings(glm(y ~ ., data = cbind(X, y = y), family = binomial())),
    predict = function(model, X, ctx) predict(model, X, type = "response"),
    preprocess = function(X_train, X_test, ctx) ta_prep_basic(X_train, X_test, one_hot = TRUE)
  )
  task <- ta_load_task("credit-g")
  r <- ta_evaluate_split(lrn, task, 0)
  expect_equal(r$metric, "roc_auc")
  expect_true(r$metric_error > 0 && r$metric_error < 0.5)
})

test_that("live results download and reproduce the reference leaderboard", {
  skip_if_offline_tests()
  reg <- ta_method_registry()
  live <- ta_results_live(reg[reg$current & reg$method %in% c("RandomForest", "LightGBM"), ], verbose = FALSE)
  expect_setequal(unique(live$method), c("RandomForest (default)", "RandomForest (tuned)",
                                         "RandomForest (tuned + ensemble)", "LightGBM (default)",
                                         "LightGBM (tuned)", "LightGBM (tuned + ensemble)"))
  expect_equal(nrow(live), 6 * 816)
  lb <- ta_leaderboard(NULL, reference = live, splits = "all", bootstrap_rounds = 0, verbose = FALSE)
  expect_equal(lb$elo[lb$method == "RandomForest (default)"], 1000)
})
