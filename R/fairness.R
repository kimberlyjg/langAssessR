#' Fairness audit: subgroup performance, calibration, and error balance
#'
#' @param pred numeric predicted probabilities or scores
#' @param y true labels (0/1 for classification)
#' @param groups data.frame or named list of subgroup vectors (same length as y);
#'               can include multiple columns (sex, race, age_group).
#' @param bins integer number of calibration bins (default 10)
#' @param threshold numeric classification threshold (default 0.5)
#' @return list(auc_by_group, calibration_bins, error_balance)
#' @export
audit_fairness <- function(pred, y, groups, bins = 10, threshold = 0.5) {
  stopifnot(length(pred) == length(y))
  if (is.data.frame(groups)) {
    gdf <- groups
  } else if (is.list(groups)) {
    gdf <- as.data.frame(groups, stringsAsFactors = FALSE)
  } else {
    gdf <- data.frame(group = groups)
    names(gdf) <- "group"
  }
  if (!all(y %in% c(0,1))) stop("audit_fairness currently supports binary outcomes (0/1).")

  # AUC helper
  auc_binary <- function(y, p) {
    o <- order(p); y <- y[o]; n1 <- sum(y == 1); n0 <- sum(y == 0)
    if (n1 == 0 || n0 == 0) return(NA_real_)
    ranks <- rank(p)
    (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }

  # A) AUC by each subgroup column
  auc_rows <- list()
  for (col in names(gdf)) {
    lv <- unique(gdf[[col]])
    for (v in lv) {
      idx <- which(gdf[[col]] == v)
      if (length(unique(y[idx])) < 2) {
        a <- NA_real_
      } else {
        a <- auc_binary(y[idx], pred[idx])
      }
      # rough 95% CI via DeLong is out-of-scope; give NA CI to stay dep-light
      auc_rows[[length(auc_rows)+1]] <- data.frame(
        subgroup = as.character(v), facet = col,
        auc = a, lcl = NA_real_, ucl = NA_real_
      )
    }
  }
  auc_by_group <- do.call(rbind, auc_rows)

  # B) Calibration bins by subgroup (only for the first grouping var for simplicity)
  calib_rows <- list()
  first_col <- names(gdf)[1]
  g1 <- gdf[[first_col]]
  # bin edges
  edges <- seq(0, 1, length.out = bins+1)
  bin_id <- cut(pred, breaks = edges, include.lowest = TRUE, labels = FALSE)
  for (v in unique(g1)) {
    idx <- which(g1 == v)
    if (length(idx) == 0) next
    for (b in 1:bins) {
      j <- idx[bin_id[idx] == b]
      if (length(j) == 0) next
      mean_pred <- mean(pred[j])
      obs_rate  <- mean(y[j])
      calib_rows[[length(calib_rows)+1]] <- data.frame(
        subgroup = as.character(v),
        prob_bin = (edges[b]+edges[b+1])/2,
        mean_pred = mean_pred, obs_rate = obs_rate
      )
    }
  }
  calibration_bins <- do.call(rbind, calib_rows)

  # C) Error balance (FNR - FPR) by subgroup (first grouping var)
  err_rows <- list()
  for (v in unique(g1)) {
    idx <- which(g1 == v)
    if (length(idx) == 0) next
    yhat <- as.integer(pred[idx] >= threshold)
    tp <- sum(yhat == 1 & y[idx] == 1)
    fp <- sum(yhat == 1 & y[idx] == 0)
    tn <- sum(yhat == 0 & y[idx] == 0)
    fn <- sum(yhat == 0 & y[idx] == 1)
    fnr <- if ((fn + tp) == 0) NA_real_ else fn / (fn + tp)
    fpr <- if ((fp + tn) == 0) NA_real_ else fp / (fp + tn)
    err_rows[[length(err_rows)+1]] <- data.frame(
      subgroup = paste0(first_col, ":", as.character(v)),
      fnr = fnr, fpr = fpr
    )
  }
  error_balance <- do.call(rbind, err_rows)

  list(
    auc_by_group = auc_by_group,
    calibration_bins = calibration_bins,
    error_balance = error_balance
  )
}
