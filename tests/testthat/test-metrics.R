test_that("ROC AUC matches a hand-computed value and handles ties", {
  y <- factor(c("neg", "neg", "pos", "pos", "pos"), levels = c("neg", "pos"))
  p <- c(0.1, 0.4, 0.35, 0.8, 0.9)
  # pairs (neg, pos): (0.1,0.35) (0.1,0.8) (0.1,0.9) (0.4,0.35) (0.4,0.8) (0.4,0.9) -> 5/6
  expect_equal(ta_metric_error(y, p, "binary"), 1 - 5 / 6)
  # ties count half
  p2 <- c(0.5, 0.5, 0.5, 0.5, 0.5)
  expect_equal(ta_metric_error(y, p2, "binary"), 0.5)
  # a probability matrix with named columns is accepted
  P <- cbind(neg = 1 - p, pos = p)
  expect_equal(ta_metric_error(y, P, "binary"), 1 - 5 / 6)
  # flipping the positive class with flipped scores gives the same AUC
  expect_equal(ta_metric_error(factor(y, levels = c("pos", "neg")), 1 - p, "binary"), 1 - 5 / 6)
})

test_that("log loss clips and renormalises like scikit-learn", {
  y <- factor(c("a", "b", "c"))
  P <- rbind(c(0.7, 0.2, 0.1), c(0.2, 0.5, 0.3), c(0.1, 0.1, 0.8))
  colnames(P) <- c("a", "b", "c")
  expect_equal(ta_metric_error(y, P, "multiclass"), -mean(log(c(0.7, 0.5, 0.8))))
  # columns are matched by name
  P2 <- P[, c("c", "a", "b")]
  expect_equal(ta_metric_error(y, P2, "multiclass"), -mean(log(c(0.7, 0.5, 0.8))))
  # zero probabilities do not blow up
  P3 <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  expect_true(is.finite(ta_metric_error(y, P3, "multiclass")))
})

test_that("RMSE is correct", {
  expect_equal(ta_metric_error(c(1, 2, 3), c(1, 2, 5), "regression"), sqrt(4 / 3))
})

test_that("prediction length is checked", {
  expect_error(ta_metric_error(c(1, 2, 3), c(1, 2), "regression"), "predictions")
})
