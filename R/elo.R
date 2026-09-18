#' Bradley-Terry Elo ratings of a results table
#'
#' Implements the TabArena Elo computation: every pair of methods "battles"
#' on every (dataset, split) unit where both have a result (lower error wins,
#' equal error is half a win each), every dataset contributes total weight 1
#' regardless of its number of splits, and the Bradley-Terry model is fitted
#' by maximum likelihood (with the same negligible ridge the reference
#' implementation uses to keep degenerate fields finite). Ratings are
#' `400 * log10(strength) + 1000`. Confidence intervals come from a
#' dataset-level bootstrap. Finally the ratings are shifted so that the
#' `calibration` method sits at `calibration_elo` (1000 for `RF (default)`
#' on the official leaderboard).
#'
#' @param results A results data.frame with one row per (method, dataset,
#'   split): columns `method`, `dataset`, `fold` (split index) and
#'   `metric_error`. Rows with `NA` errors are dropped.
#' @param calibration Method whose Elo is pinned to `calibration_elo`; `NULL`
#'   leaves the ratings centred on `init_rating`.
#' @param calibration_elo The pinned rating.
#' @param bootstrap_rounds Number of dataset-level bootstrap resamples used
#'   for the 95% interval (`elo_plus` / `elo_minus`). `0` or `1` skips the
#'   bootstrap.
#' @param seed Seed of the bootstrap.
#' @param scale,init_rating Elo scale and centre (400 and 1000).
#' @param post_calibrate If `TRUE` (the reference behaviour) the bootstrap and
#'   the point estimate are computed uncalibrated, the interval widths are
#'   derived from them, and only the point estimate is shifted to the
#'   calibration afterwards, so the widths do not depend on the calibration
#'   method. If `FALSE` every bootstrap draw is calibrated individually.
#' @param method_col,task_col,split_col,error_col Column names.
#' @return A data.frame with `method`, `elo`, `elo_plus`, `elo_minus`
#'   (upper / lower 95% interval half-widths) and `elo_boot_median`, sorted by
#'   `elo` decreasing. The bootstrap draws are attached as attribute
#'   `"bootstrap"` (a matrix rounds x methods).
#' @examples
#' res <- expand.grid(method = c("A", "B", "C"), dataset = paste0("d", 1:5), fold = 0:2,
#'                    stringsAsFactors = FALSE)
#' set.seed(1)
#' res$metric_error <- runif(nrow(res)) + ifelse(res$method == "A", -0.3, 0)
#' ta_elo(res, calibration = "B", bootstrap_rounds = 20)
#' @export
ta_elo <- function(results, calibration = "RF (default)", calibration_elo = 1000,
                   bootstrap_rounds = 100, seed = 0, scale = 400, init_rating = 1000,
                   post_calibrate = TRUE, method_col = "method", task_col = "dataset",
                   split_col = "fold", error_col = "metric_error") {
  b <- ta_battles(results, method_col = method_col, task_col = task_col,
                  split_col = split_col, error_col = error_col)
  n <- length(b$methods)
  if (n < 2) stop("Elo needs at least two methods.", call. = FALSE)
  if (!is.null(calibration) && !calibration %in% b$methods) {
    stop("Calibration method '", calibration, "' has no results.", call. = FALSE)
  }
  n_tasks <- length(b$tasks)
  M <- matrix(b$W, nrow = n_tasks)  # n_tasks x (n * n)
  fit_counts <- function(counts, start = NULL) {
    Wtot <- matrix(as.numeric(counts %*% M), n, n)
    ta_bt_fit(Wtot, scale = scale, init_rating = init_rating, start = start)
  }
  calibrate <- function(elo, target_method) {
    if (is.null(target_method)) return(elo)
    elo + (calibration_elo - elo[[target_method]])
  }
  draw_calibration <- if (post_calibrate) NULL else calibration
  point <- fit_counts(rep(1, n_tasks))
  point_elo <- calibrate(stats::setNames(point$elo, b$methods), draw_calibration)
  boot <- NULL
  elo_plus <- elo_minus <- rep(0, n)
  boot_median <- point_elo
  if (bootstrap_rounds > 1) {
    set.seed(seed)
    boot <- matrix(NA_real_, bootstrap_rounds, n, dimnames = list(NULL, b$methods))
    start <- point$t
    for (r in seq_len(bootstrap_rounds)) {
      counts <- tabulate(sample.int(n_tasks, n_tasks, replace = TRUE), nbins = n_tasks)
      fit <- fit_counts(counts, start = start)
      start <- fit$t
      boot[r, ] <- calibrate(stats::setNames(fit$elo, b$methods), draw_calibration)
    }
    q <- apply(boot, 2, stats::quantile, probs = c(0.025, 0.5, 0.975), names = FALSE)
    boot_median <- q[2, ]
    elo_plus <- pmax(q[3, ] - point_elo, 0)
    elo_minus <- pmax(point_elo - q[1, ], 0)
  }
  if (post_calibrate) {
    offset <- if (is.null(calibration)) 0 else calibration_elo - point_elo[[calibration]]
    point_elo <- point_elo + offset
    boot_median <- boot_median + offset
    if (!is.null(boot)) boot <- boot + offset
  }
  out <- data.frame(
    method = b$methods, elo = as.numeric(point_elo), elo_plus = as.numeric(elo_plus),
    elo_minus = as.numeric(elo_minus), elo_boot_median = as.numeric(boot_median),
    stringsAsFactors = FALSE
  )
  out <- out[order(-out$elo), , drop = FALSE]
  rownames(out) <- NULL
  attr(out, "bootstrap") <- boot
  attr(out, "n_tasks") <- n_tasks
  out
}

# Per-task pairwise win matrices. Returns list(methods, tasks, W) with W an
# array [task, i, j] holding the weighted number of wins of method i over j
# on that task (ties count half; each task has total weight 1 across splits).
ta_battles <- function(results, method_col = "method", task_col = "dataset",
                       split_col = "fold", error_col = "metric_error") {
  need <- c(method_col, task_col, error_col)
  miss <- setdiff(need, names(results))
  if (length(miss)) stop("Results are missing columns: ", paste(miss, collapse = ", "), call. = FALSE)
  df <- results[!is.na(results[[error_col]]), , drop = FALSE]
  has_split <- !is.null(split_col) && split_col %in% names(df)
  split <- if (has_split) df[[split_col]] else rep(0L, nrow(df))
  key <- paste(df[[method_col]], df[[task_col]], split, sep = "\r")
  if (anyDuplicated(key)) {
    stop("Duplicate (method, task, split) rows in results; aggregate them first.", call. = FALSE)
  }
  methods <- sort(unique(as.character(df[[method_col]])))
  tasks <- sort(unique(as.character(df[[task_col]])))
  n <- length(methods)
  W <- array(0, dim = c(length(tasks), n, n))
  m_idx <- match(as.character(df[[method_col]]), methods)
  t_idx <- match(as.character(df[[task_col]]), tasks)
  err <- as.numeric(df[[error_col]])
  units <- split(seq_len(nrow(df)), list(t_idx, split), drop = TRUE)
  n_splits_task <- tapply(split, t_idx, function(s) length(unique(s)))
  for (u in units) {
    if (length(u) < 2) next
    ti <- t_idx[u[1]]
    w <- 1 / n_splits_task[[as.character(ti)]]
    e <- err[u]
    wins <- outer(e, e, "<") + 0.5 * outer(e, e, "==")
    diag(wins) <- 0
    ii <- m_idx[u]
    W[ti, ii, ii] <- W[ti, ii, ii] + w * wins
  }
  list(methods = methods, tasks = tasks, W = W)
}

# Ridge matching the reference implementation's LogisticRegression(C = 1e6)
# penalty expressed on natural-log strengths.
ta_bt_ridge <- function() 0.5 / (1e6 * log(10)^2)

# Maximum-likelihood Bradley-Terry fit from a total win matrix.
ta_bt_fit <- function(W, scale = 400, init_rating = 1000, start = NULL, ridge = ta_bt_ridge()) {
  n <- nrow(W)
  N <- W + t(W)
  wins <- rowSums(W)
  up <- upper.tri(N)
  logsum <- function(t) {
    m <- pmax(outer(t, rep(1, n)), outer(rep(1, n), t))
    m + log(exp(outer(t, rep(1, n)) - m) + exp(outer(rep(1, n), t) - m))
  }
  fn <- function(t) {
    L <- logsum(t)
    ll <- sum(wins * t) - sum(N[up] * L[up])
    -ll + ridge * sum(t^2)
  }
  gr <- function(t) {
    L <- logsum(t)
    P <- exp(outer(t, rep(1, n)) - L)
    diag(P) <- 0
    g <- wins - rowSums(N * P)
    -g + 2 * ridge * t
  }
  par0 <- if (is.null(start)) rep(0, n) else as.numeric(start)
  fit <- stats::optim(par0, fn, gr, method = "L-BFGS-B",
                      control = list(maxit = 10000L, factr = 10, pgtol = 1e-10))
  t <- fit$par
  t <- t - mean(t)
  list(t = t, elo = scale * t / log(10) + init_rating, convergence = fit$convergence)
}

#' Expected win probability from two Elo ratings
#'
#' @param elo_a,elo_b Ratings.
#' @param scale Elo scale (400).
#' @return The probability that `a` beats `b`.
#' @export
ta_elo_win_prob <- function(elo_a, elo_b, scale = 400) {
  1 / (1 + 10^((elo_b - elo_a) / scale))
}
