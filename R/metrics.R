#' Official TabArena metric error
#'
#' Computes the *error* (lower is better) TabArena scores a prediction with:
#' `1 - ROC AUC` for binary classification, log loss for multiclass
#' classification and RMSE for regression.
#'
#' @param y_true Observed targets: a factor (classification) or numeric vector
#'   (regression).
#' @param pred Predictions. Regression: a numeric vector. Binary: a numeric
#'   vector with the probability of the positive class (the second factor
#'   level), or a matrix/data.frame of class probabilities with one column per
#'   class. Multiclass: a matrix/data.frame of class probabilities with one
#'   column per class (columns are matched by name when named, otherwise they
#'   are assumed to follow `levels`).
#' @param problem_type One of `"binary"`, `"multiclass"`, `"regression"`.
#' @param levels Class labels (defaults to `levels(y_true)`).
#' @param metric Optional metric name; defaults to the official metric of the
#'   problem type (`"roc_auc"`, `"log_loss"`, `"rmse"`).
#' @return A single non-negative number.
#' @examples
#' y <- factor(c("a", "b", "b", "a"))
#' ta_metric_error(y, c(0.1, 0.9, 0.6, 0.4), "binary")
#' @export
ta_metric_error <- function(y_true, pred, problem_type, levels = NULL, metric = NULL) {
  if (is.null(metric)) metric <- ta_default_metric(problem_type)
  switch(metric,
    roc_auc = 1 - ta_roc_auc(y_true, pred, levels = levels),
    log_loss = ta_log_loss(y_true, pred, levels = levels),
    rmse = ta_rmse(y_true, pred),
    stop("Unsupported metric: ", metric, call. = FALSE)
  )
}

ta_default_metric <- function(problem_type) {
  switch(problem_type,
    binary = "roc_auc", multiclass = "log_loss", regression = "rmse",
    stop("Unknown problem type: ", problem_type, call. = FALSE)
  )
}

ta_rmse <- function(y_true, pred) {
  y_true <- as.numeric(y_true)
  pred <- as.numeric(pred)
  ta_check_pred_length(y_true, pred)
  if (anyNA(pred)) stop("Predictions contain missing values.", call. = FALSE)
  sqrt(mean((y_true - pred)^2))
}

# Probability of the positive class from a vector or a probability matrix.
ta_positive_prob <- function(pred, levels) {
  if (is.data.frame(pred)) pred <- as.matrix(pred)
  if (is.matrix(pred)) {
    if (ncol(pred) == 1) return(as.numeric(pred[, 1]))
    if (!is.null(colnames(pred)) && levels[2] %in% colnames(pred)) return(as.numeric(pred[, levels[2]]))
    if (ncol(pred) == 2) return(as.numeric(pred[, 2]))
    stop("Cannot identify the positive-class column in the prediction matrix.", call. = FALSE)
  }
  as.numeric(pred)
}

# ROC AUC via the Mann-Whitney statistic (average ranks handle ties, which
# matches the trapezoidal AUC of scikit-learn).
ta_roc_auc <- function(y_true, pred, levels = NULL) {
  y_true <- ta_as_factor(y_true, levels)
  levels <- levels(y_true)
  if (length(levels) != 2) stop("ROC AUC needs exactly two classes.", call. = FALSE)
  p <- ta_positive_prob(pred, levels)
  ta_check_pred_length(y_true, p)
  if (anyNA(p)) stop("Predictions contain missing values.", call. = FALSE)
  pos <- y_true == levels[2]
  n_pos <- sum(pos)
  n_neg <- sum(!pos)
  if (n_pos == 0 || n_neg == 0) return(NA_real_)
  r <- rank(p, ties.method = "average")
  (sum(r[pos]) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg)
}

# Multiclass log loss with probabilities clipped to [eps, 1 - eps] and rows
# renormalised, following scikit-learn.
ta_log_loss <- function(y_true, pred, levels = NULL, eps = 1e-15) {
  y_true <- ta_as_factor(y_true, levels)
  levels <- levels(y_true)
  P <- ta_prob_matrix(pred, levels)
  ta_check_pred_length(y_true, P[, 1])
  if (anyNA(P)) stop("Predictions contain missing values.", call. = FALSE)
  P <- pmin(pmax(P, eps), 1 - eps)
  P <- P / rowSums(P)
  idx <- cbind(seq_len(nrow(P)), as.integer(y_true))
  -mean(log(P[idx]))
}

ta_prob_matrix <- function(pred, levels) {
  if (is.data.frame(pred)) pred <- as.matrix(pred)
  if (!is.matrix(pred)) {
    if (length(levels) == 2) {
      p <- as.numeric(pred)
      pred <- cbind(1 - p, p)
      colnames(pred) <- levels
    } else {
      stop("Multiclass predictions must be a matrix of class probabilities.", call. = FALSE)
    }
  }
  storage.mode(pred) <- "double"
  if (ncol(pred) != length(levels)) {
    stop("Prediction matrix has ", ncol(pred), " columns but the task has ", length(levels), " classes.", call. = FALSE)
  }
  if (!is.null(colnames(pred))) {
    missing_cols <- setdiff(levels, colnames(pred))
    if (length(missing_cols) == 0) {
      pred <- pred[, levels, drop = FALSE]
    } else {
      warning("Prediction columns are not named after the class labels; assuming level order.", call. = FALSE)
    }
  }
  pred
}

ta_as_factor <- function(y, levels = NULL) {
  if (is.factor(y)) {
    if (!is.null(levels)) y <- factor(as.character(y), levels = levels)
    return(y)
  }
  if (is.null(levels)) levels <- sort(unique(as.character(y)))
  factor(as.character(y), levels = levels)
}

ta_check_pred_length <- function(y_true, pred) {
  if (length(pred) != length(y_true)) {
    stop("Got ", length(pred), " predictions for ", length(y_true), " test rows.", call. = FALSE)
  }
  invisible(TRUE)
}
