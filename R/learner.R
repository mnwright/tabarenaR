#' Wrap a train/predict pair as a TabArena learner
#'
#' Any R model can be benchmarked by supplying two functions.
#'
#' `train(X, y, ctx)` receives the training features `X` (a data.frame with
#' numeric and factor columns, possibly containing `NA`), the training target
#' `y` (a factor for classification, numeric for regression) and a context
#' list `ctx` with `dataset`, `task_id`, `split`, `repeat`, `fold`,
#' `problem_type`, `metric`, `target`, `levels` and `positive_class`. It must
#' return a fitted model object.
#'
#' `predict(model, X, ctx)` receives that model and the test features and must
#' return, depending on `ctx$problem_type`:
#' * `"regression"`: a numeric vector of predictions;
#' * `"binary"`: a numeric vector with the probability of `ctx$positive_class`
#'   (the second level of `ctx$levels`), or a probability matrix with one
#'   column per class;
#' * `"multiclass"`: a matrix (or data.frame) of class probabilities with one
#'   column per class, ideally with columns named after `ctx$levels`.
#'
#' @param train A function `function(X, y, ctx)`.
#' @param predict A function `function(model, X, ctx)`.
#' @param name The method name shown on the leaderboard.
#' @param preprocess Optional function `function(X_train, X_test, ctx)`
#'   returning `list(X_train = , X_test = )`, applied before `train` and
#'   `predict`. See [ta_prep_basic()] for a ready-made one.
#' @return An object of class `ta_learner`.
#' @examples
#' \dontrun{
#' rf <- ta_learner(
#'   name = "ranger (R)",
#'   train = function(X, y, ctx) {
#'     ranger::ranger(x = X, y = y, probability = ctx$problem_type != "regression", num.threads = 4)
#'   },
#'   predict = function(model, X, ctx) {
#'     p <- predict(model, data = X, num.threads = 4)$predictions
#'     if (ctx$problem_type == "regression") p else p  # matrix of class probabilities
#'   },
#'   preprocess = ta_prep_basic
#' )
#' res <- ta_evaluate(rf, tasks = ta_tasks("tiny"), splits = "lite")
#' ta_leaderboard(res)
#' }
#' @export
ta_learner <- function(train, predict, name = "my_method", preprocess = NULL) {
  stopifnot(is.function(train), is.function(predict))
  if (!is.null(preprocess)) stopifnot(is.function(preprocess))
  structure(list(train = train, predict = predict, name = name, preprocess = preprocess),
            class = "ta_learner")
}

#' @export
print.ta_learner <- function(x, ...) {
  cat("<TabArena learner> ", x$name, if (!is.null(x$preprocess)) " (with preprocessing)" else "", "\n", sep = "")
  invisible(x)
}

#' Basic preprocessing for learners that cannot handle missing values
#'
#' Median-imputes numeric columns and adds an explicit `"(missing)"` level to
#' factors, using training statistics only. Character and logical columns are
#' converted to factors, factor levels are aligned between train and test, and
#' constant columns are dropped. Optionally one-hot encodes factors.
#'
#' @param X_train,X_test Feature data.frames.
#' @param ctx The split context (unused, accepted for compatibility with
#'   [ta_learner()]'s `preprocess` slot).
#' @param one_hot If `TRUE`, factors are expanded into 0/1 indicator columns so
#'   the result is purely numeric.
#' @param max_levels Factors with more levels than this are dropped when
#'   `one_hot = TRUE` (to avoid gigantic design matrices).
#' @return `list(X_train = , X_test = )`.
#' @export
ta_prep_basic <- function(X_train, X_test, ctx = NULL, one_hot = FALSE, max_levels = 100L) {
  to_factor <- function(x) if (is.character(x) || is.logical(x)) factor(x) else x
  X_train[] <- lapply(X_train, to_factor)
  X_test[] <- lapply(X_test, to_factor)
  keep <- character()
  for (nm in names(X_train)) {
    tr <- X_train[[nm]]
    te <- X_test[[nm]]
    if (is.factor(tr)) {
      lv <- levels(droplevels(tr))
      te_chr <- as.character(te)
      te_chr[!is.na(te_chr) & !te_chr %in% lv] <- NA
      has_na <- anyNA(tr) || anyNA(te_chr)
      if (has_na) lv <- c(lv, "(missing)")
      tr_chr <- as.character(tr)
      tr_chr[is.na(tr_chr)] <- "(missing)"
      te_chr[is.na(te_chr)] <- "(missing)"
      tr <- factor(tr_chr, levels = lv)
      te <- factor(te_chr, levels = lv)
      if (length(lv) < 2) next
      if (one_hot) {
        if (length(lv) > max_levels) next
        for (l in lv) {
          X_train[[paste0(nm, "=", l)]] <- as.numeric(tr == l)
          X_test[[paste0(nm, "=", l)]] <- as.numeric(te == l)
          keep <- c(keep, paste0(nm, "=", l))
        }
        next
      }
    } else {
      tr <- suppressWarnings(as.numeric(tr))
      te <- suppressWarnings(as.numeric(te))
      med <- stats::median(tr, na.rm = TRUE)
      if (is.na(med)) next
      tr[is.na(tr)] <- med
      te[is.na(te)] <- med
      if (length(unique(tr)) < 2) next
    }
    X_train[[nm]] <- tr
    X_test[[nm]] <- te
    keep <- c(keep, nm)
  }
  list(X_train = X_train[, keep, drop = FALSE], X_test = X_test[, keep, drop = FALSE])
}
