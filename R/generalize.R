#' Cross-context validation (k-fold or leave-site-out)
#'
#' Evaluate generalizability across sites/contexts. By default, runs
#' leave-site-out (LSO) CV: train on all sites except one, test on the held-out site.
#'
#' @param x data.frame or matrix of features (rows = samples)
#' @param y numeric or factor outcome. Binary classification if 0/1 or 2-level factor; else regression.
#' @param site factor/character vector of same length as y denoting site/context
#' @param leave_site_out logical; if TRUE, run LSO-CV. Default TRUE.
#' @param k integer; pooled k-fold CV folds (for comparator). Default 5.
#' @param train_fun function(x_train, y_train) -> model. Default = glm-based fast baseline.
#' @param predict_fun function(model, x_test) -> numeric predictions (prob for classification).
#' @param seed integer RNG seed for reproducibility
#' @return list with site_estimates, compare, resamples
#' @export
cross_context_cv <- function(x, y, site,
                             leave_site_out = TRUE, k = 5,
                             train_fun = NULL, predict_fun = NULL,
                             seed = 123) {

  stopifnot(NROW(x) == length(y), length(y) == length(site))
  x <- as.data.frame(x)
  site <- as.character(site)

  # auto type
  type <- if ((is.factor(y) && length(levels(y)) == 2) || all(y %in% c(0,1))) "classification" else "regression"
  if (is.factor(y) && type == "classification") y <- as.integer(y == levels(y)[2])

  # defaults: fast GLM baselines
  default_train <- function(x_tr, y_tr) {
    df <- cbind.data.frame(y = y_tr, x_tr)
    if (type == "classification") {
      glm(y ~ ., data = df, family = stats::binomial())
    } else {
      glm(y ~ ., data = df)
    }
  }
  default_predict <- function(mod, x_te) {
    if (type == "classification") {
      as.numeric(stats::predict(mod, newdata = x_te, type = "response"))
    } else {
      as.numeric(stats::predict(mod, newdata = x_te))
    }
  }
  if (is.null(train_fun))   train_fun   <- default_train
  if (is.null(predict_fun)) predict_fun <- default_predict

  # metric helpers
  auc_binary <- function(y_true, p) {
    o <- order(p); y2 <- y_true[o]
    n1 <- sum(y2 == 1); n0 <- sum(y2 == 0)
    if (n1 == 0 || n0 == 0) return(NA_real_)
    ranks <- rank(p)
    (sum(ranks[y2 == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  r2_reg <- function(y_true, pred) {
    ss_res <- sum((y_true - pred)^2); ss_tot <- sum((y_true - mean(y_true))^2)
    1 - ss_res/ss_tot
  }
  metric_fun <- if (type == "classification") auc_binary else r2_reg
  metric_lab <- if (type == "classification") "AUROC" else "R2"

  # Leave-site-out CV
  lso_rows <- list()
  lso_raw  <- list()
  if (leave_site_out) {
    for (s in unique(site)) {
      test_idx  <- which(site == s)
      train_idx <- setdiff(seq_along(y), test_idx)
      if (length(unique(y[train_idx])) < 2 && type == "classification") next

      mod  <- train_fun(x[train_idx, , drop = FALSE], y[train_idx])
      pred <- predict_fun(mod, x[test_idx, , drop = FALSE])
      est  <- metric_fun(y[test_idx], pred)

      # crude CI
      lcl <- est - 0.05
      ucl <- est + 0.05

      lso_rows[[length(lso_rows)+1]] <- data.frame(site = s, estimate = est, lcl = lcl, ucl = ucl)
      lso_raw[[s]] <- list(site = s, test_index = test_idx, estimate = est)
    }
  }
  site_estimates <- if (length(lso_rows)) do.call(rbind, lso_rows) else NULL

  # Pooled k-fold CV
  pooled_row <- NULL
  pooled_raw <- NULL
  if (k > 1) {
    set.seed(seed)
    folds <- sample(rep(1:k, length.out = length(y)))
    ests  <- numeric(k)
    for (f in 1:k) {
      tr <- which(folds != f); te <- which(folds == f)
      if (type == "classification" && length(unique(y[tr])) < 2) { ests[f] <- NA_real_; next }
      mod  <- train_fun(x[tr, , drop = FALSE], y[tr])
      pred <- predict_fun(mod, x[te, , drop = FALSE])
      ests[f] <- metric_fun(y[te], pred)
    }
    est <- mean(ests, na.rm = TRUE)
    lcl <- est - 0.05; ucl <- est + 0.05
    pooled_row <- data.frame(site = "Overall", estimate = est, lcl = lcl, ucl = ucl, model = "Pooled CV")
    pooled_raw <- list(fold_estimates = ests)
  }

  # Compare table
  compare <- NULL
  if (!is.null(site_estimates)) {
    lso_cmp <- transform(site_estimates, model = "Leave-site-out")
    compare <- rbind(lso_cmp, pooled_row)
  } else if (!is.null(pooled_row)) {
    compare <- pooled_row
  }

  list(
    site_estimates = site_estimates,
    compare = compare,
    resamples = list(lso = lso_raw, pooled = pooled_raw),
    metric = metric_lab
  )
}
