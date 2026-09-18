paper_subset <- function(datasets = c("credit-g", "diabetes", "airfoil_self_noise", "anneal"),
                         methods = c("RF (default)", "GBM (default)", "CAT (default)", "XT (default)", "KNN (default)")) {
  res <- ta_results_paper()
  res[res$dataset %in% datasets & res$method %in% methods, ]
}

test_that("a reference-only leaderboard pins RF (default) to 1000", {
  ref <- paper_subset()
  lb <- ta_leaderboard(NULL, reference = ref, splits = "all", bootstrap_rounds = 10, verbose = FALSE)
  expect_s3_class(lb, "ta_leaderboard")
  expect_equal(nrow(lb), 5)
  expect_equal(lb$elo[lb$method == "RF (default)"], 1000)
  expect_equal(lb$position, 1:5)
  expect_true(all(diff(lb$elo) <= 0))
  expect_true(all(lb$score >= 0 & lb$score <= 1))
  expect_true(all(lb$winrate >= 0 & lb$winrate <= 1))
  expect_equal(unique(lb$imputed_pct), 0)
  expect_equal(unique(lb$n_splits), 4 * 30)
  per_task <- attr(lb, "per_task")
  expect_equal(nrow(per_task), 5 * 120)
})

test_that("a new method is scored on the splits it evaluated", {
  ref <- paper_subset()
  new <- ref[ref$method == "GBM (default)" & ref$fold == 0, ]
  new$method <- "mine"
  new$metric_error <- new$metric_error * 0.999  # slightly better than GBM
  lb <- ta_leaderboard(new, reference = ref, bootstrap_rounds = 10, verbose = FALSE)
  expect_true("mine" %in% lb$method)
  expect_true(lb$is_new[lb$method == "mine"])
  expect_equal(unique(lb$n_splits), 4)  # TabArena-Lite on 4 datasets
  expect_true(lb$elo[lb$method == "mine"] > lb$elo[lb$method == "GBM (default)"])
  expect_equal(lb$elo[lb$method == "RF (default)"], 1000)
})

test_that("missing results are dropped or imputed", {
  ref <- paper_subset()
  new <- ref[ref$method == "GBM (default)" & ref$dataset %in% c("credit-g", "diabetes"), ]
  new$method <- "partial"
  # the field is all splits; 'partial' lacks two datasets
  lb <- ta_leaderboard(new, reference = ref, splits = "all", bootstrap_rounds = 5, verbose = FALSE)
  expect_false("partial" %in% lb$method)
  expect_equal(attr(lb, "dropped"), "partial")
  lb2 <- ta_leaderboard(new, reference = ref, splits = "all", imputation = TRUE, bootstrap_rounds = 5, verbose = FALSE)
  expect_true("partial" %in% lb2$method)
  expect_equal(lb2$imputed_pct[lb2$method == "partial"], 50)
  expect_equal(lb2$n_datasets[lb2$method == "partial"], 4)
  # NA errors count as missing
  new2 <- new
  new2$metric_error[1] <- NA
  lb3 <- ta_leaderboard(new2, reference = ref, bootstrap_rounds = 5, verbose = FALSE)
  expect_equal(unique(lb3$n_splits), nrow(new) - 1)
})

test_that("results with extra or renamed columns are accepted", {
  ref <- paper_subset()
  new <- data.frame(framework = "x", dataset = "credit-g", split = 0:2, metric_error = c(0.2, 0.3, 0.25))
  lb <- ta_leaderboard(new, reference = ref, bootstrap_rounds = 0, verbose = FALSE)
  expect_true("x" %in% lb$method)
  expect_equal(unique(lb$n_splits), 3)
})
