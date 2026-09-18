make_results <- function(seed = 1, methods = c("A", "B", "C", "D"), n_datasets = 12, n_splits = 3) {
  set.seed(seed)
  res <- expand.grid(method = methods, dataset = paste0("d", seq_len(n_datasets)),
                     fold = seq_len(n_splits) - 1L, stringsAsFactors = FALSE)
  skill <- stats::setNames(seq_along(methods) * 0.15, methods)
  res$metric_error <- runif(nrow(res)) * 0.3 + skill[res$method]
  res
}

test_that("Elo orders methods by strength and pins the calibration method", {
  res <- make_results()
  elo <- ta_elo(res, calibration = "B", bootstrap_rounds = 30, seed = 1)
  expect_equal(elo$method, c("A", "B", "C", "D"))
  expect_equal(elo$elo[elo$method == "B"], 1000)
  expect_true(all(elo$elo_plus >= 0) && all(elo$elo_minus >= 0))
  expect_equal(dim(attr(elo, "bootstrap")), c(30, 4))
})

test_that("Bradley-Terry fit reproduces a known two-method solution", {
  # A beats B on 3 of 4 tasks: p = 0.75 -> rating gap = 400 * log10(3)
  res <- data.frame(method = rep(c("A", "B"), each = 4), dataset = rep(paste0("d", 1:4), 2),
                    fold = 0L, metric_error = c(0.1, 0.1, 0.1, 0.9, 0.5, 0.5, 0.5, 0.5))
  elo <- ta_elo(res, calibration = NULL, bootstrap_rounds = 0)
  gap <- elo$elo[elo$method == "A"] - elo$elo[elo$method == "B"]
  expect_equal(gap, 400 * log10(3), tolerance = 1e-4)
  expect_equal(mean(elo$elo), 1000, tolerance = 1e-6)
})

test_that("tasks are weighted equally regardless of their number of splits", {
  # Dataset d1 has 10 splits where B wins, d2 has 1 split where A wins:
  # with equal task weights the two methods are tied.
  res <- rbind(
    data.frame(method = "A", dataset = "d1", fold = 0:9, metric_error = 0.9),
    data.frame(method = "B", dataset = "d1", fold = 0:9, metric_error = 0.1),
    data.frame(method = "A", dataset = "d2", fold = 0, metric_error = 0.1),
    data.frame(method = "B", dataset = "d2", fold = 0, metric_error = 0.9)
  )
  elo <- ta_elo(res, calibration = NULL, bootstrap_rounds = 0)
  expect_equal(diff(elo$elo), 0, tolerance = 1e-6)
})

test_that("identical results give identical ratings and ties count half", {
  res <- make_results()
  res$metric_error <- 0.5
  elo <- ta_elo(res, calibration = "A", bootstrap_rounds = 0)
  expect_true(all(abs(elo$elo - 1000) < 1e-6))
})

test_that("degenerate fields stay finite", {
  res <- make_results(methods = c("A", "B"))
  res$metric_error <- ifelse(res$method == "A", 0, 1)  # A wins everything
  elo <- ta_elo(res, calibration = "B", bootstrap_rounds = 5)
  expect_true(all(is.finite(elo$elo)))
})

test_that("duplicate rows are rejected", {
  res <- make_results()
  expect_error(ta_elo(rbind(res, res[1, ]), calibration = "A", bootstrap_rounds = 0), "Duplicate")
})

test_that("win probability formula", {
  expect_equal(ta_elo_win_prob(1000, 1000), 0.5)
  expect_equal(ta_elo_win_prob(1400, 1000), 10 / 11)
})
