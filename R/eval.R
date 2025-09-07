#' Evaluate model predictions (classification or regression)
#'
#' @param pred numeric; predicted probabilities/scores (classification) or predictions (regression)
#' @param y    numeric or factor; ground truth. For classification, y must be 0/1 or a two-level factor
#' @param type character; "classification" or "regression". If NULL, auto-detect
#' @param threshold numeric; classification threshold (default 0.5)
#' @param calib_bins integer; number of calibration bins for classification (default 10; set 0 to skip)
#' @return A list with:
#'   \item{summary}{data.frame/tibble of scalar metrics}
#'   \item{calibration}{(classification only) calibration bins data.frame}
#'   \item{confusion}{(classification only) confusion counts at threshold}
#'   \item{residuals}{(regression only) residuals dataframe}
#' @export
la_eval <- function(pred, y, type = NULL, threshold = 0.5, calib_bins = 10) {
  stopifnot(length(pred) == length(y))
  n <- length(y)

  # auto-detect type
  if (is.null(type)) {
    if (is.factor(y) && length(levels(y)) == 2) {
      type <- "classification"
      y <- as.integer(y == levels(y)[2])  # assume positive is the second level
    } else if (all(y %in% c(0, 1))) {
      type <- "classification"
    } else {
      type <- "regression"
    }
  }

  if (type == "classification") {
    # make sure y is 0/1
    if (is.factor(y)) y <- as.integer(y == levels(y)[2])
    stopifnot(all(y %in% c(0, 1)))

    # ---- metrics ----
    auc <- .auc_binary(y, pred)
    yhat <- as.integer(pred >= threshold)

    tp <- sum(yhat == 1 & y == 1)
    tn <- sum(yhat == 0 & y == 0)
    fp <- sum(yhat == 1 & y == 0)
    fn <- sum(yhat == 0 & y == 1)

    acc <- (tp + tn) / n
    prec <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    rec  <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)
    f1   <- if (is.na(prec) || is.na(rec) || (prec + rec) == 0) NA_real_ else 2 * prec * rec / (prec + rec)

    # calibration bins
    calibration <- NULL
    if (!is.null(calib_bins) && calib_bins > 0) {
      edges <- seq(0, 1, length.out = calib_bins + 1)
      bin_id <- cut(pred, breaks = edges, include.lowest = TRUE, labels = FALSE)
      calibration <- lapply(1:calib_bins, function(b) {
        idx <- which(bin_id == b)
        if (length(idx) == 0) return(NULL)
        data.frame(
          prob_bin  = (edges[b] + edges[b+1]) / 2,
          mean_pred = mean(pred[idx]),
          obs_rate  = mean(y[idx]),
          n = length(idx)
        )
      })
      calibration <- do.call(rbind, calibration)
    }

    out <- list(
      summary = data.frame(
        metric = c("AUROC", "Accuracy", "Precision", "Recall", "F1"),
        value  = round(c(auc, acc, prec, rec, f1), 4),
        stringsAsFactors = FALSE
      ),
      calibration = calibration,
      confusion = data.frame(tp = tp, fp = fp, tn = tn, fn = fn)
    )

  } else if (type == "regression") {
    # ---- metrics ----
    y <- as.numeric(y)
    rmse <- sqrt(mean((pred - y)^2))
    mae  <- mean(abs(pred - y))
    r2   <- .r2_reg(y, pred)

    out <- list(
      summary = data.frame(
        metric = c("RMSE", "MAE", "R2"),
        value  = round(c(rmse, mae, r2), 4),
        stringsAsFactors = FALSE
      ),
      residuals = data.frame(
        y = y, pred = pred, resid = y - pred
      )
    )
  } else {
    stop("type must be 'classification' or 'regression'")
  }

  out
}

# ---- helpers ----

# robust AUROC via Mann-Whitney U (no hard deps)
.auc_binary <- function(y, p) {
  o <- order(p); y <- y[o]
  n1 <- sum(y == 1); n0 <- sum(y == 0)
  if (n1 == 0 || n0 == 0) return(NA_real_)
  ranks <- rank(p)
  (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

.r2_reg <- function(y, yhat) {
  ss_res <- sum((y - yhat)^2)
  ss_tot <- sum((y - mean(y))^2)
  1 - ss_res / ss_tot
}

#' Decision-curve analysis (binary outcomes)
#'
#' @param pred numeric predicted probabilities
#' @param y    0/1 labels
#' @param thresholds numeric vector of threshold probabilities
#' @return data.frame with threshold, net_benefit, policy
#' @export
decision_curve <- function(pred, y, thresholds = seq(0.01, 0.8, by = 0.01)) {
  stopifnot(length(pred) == length(y), all(y %in% c(0, 1)))
  n <- length(y)

  # Treat-all and treat-none policies
  nb_none <- rep(0, length(thresholds))
  nb_all  <- sapply(thresholds, function(pt) {
    # Net benefit treat-all = prevalence - (1 - prevalence) * (pt / (1 - pt))
    prev <- mean(y == 1)
    prev - (1 - prev) * (pt / (1 - pt))
  })

  # Model policy
  nb_model <- sapply(thresholds, function(pt) {
    yhat <- as.integer(pred >= pt)
    tp <- sum(yhat == 1 & y == 1)
    fp <- sum(yhat == 1 & y == 0)
    w  <- pt / (1 - pt)
    (tp / n) - (fp / n) * w
  })

  rbind(
    data.frame(threshold = thresholds, net_benefit = nb_model, policy = "Model"),
    data.frame(threshold = thresholds, net_benefit = nb_all,   policy = "Treat All"),
    data.frame(threshold = thresholds, net_benefit = nb_none,  policy = "Treat None")
  )
}
