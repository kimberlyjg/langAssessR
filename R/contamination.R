#' Check criterion contamination (Mirror vs Non-Mirror)
#'
#' Compare in-sample and external-criterion performance for mirror vs non-mirror
#' predictions and flag inflation risk.
#'
#' @param in_mirror numeric, in-sample predicted scores from the Mirror model
#' @param in_nonmirror numeric, in-sample predicted scores from the Non-Mirror model
#' @param y_in numeric, in-sample ground truth (0/1 for classification or continuous)
#' @param ext_mirror numeric, external-criterion predictions from the Mirror model
#' @param ext_nonmirror numeric, external-criterion predictions from the Non-Mirror model
#' @param y_ext numeric, external criterion ground truth (0/1 or continuous)
#' @param metric character, "auc" (binary) or "r2" (continuous). If NULL, auto-detect from y.
#' @param delta_thresh numeric, threshold for flagging large in-sample inflation (default .05)
#' @return data.frame with metrics and a risk flag
#' @export
check_contamination <- function(in_mirror, in_nonmirror, y_in,
                                ext_mirror, ext_nonmirror, y_ext,
                                metric = NULL, delta_thresh = 0.05) {

  stopifnot(length(in_mirror) == length(y_in),
            length(in_nonmirror) == length(y_in),
            length(ext_mirror) == length(y_ext),
            length(ext_nonmirror) == length(y_ext))

  # auto-detect metric
  if (is.null(metric)) {
    metric <- if (all(y_in %in% c(0, 1)) && all(y_ext %in% c(0, 1))) "auc" else "r2"
  }

  # helpers
  auc_binary <- function(y, p) {
    # Mann-Whitney U based AUC; robust to ties
    o <- order(p); y <- y[o]; n1 <- sum(y == 1); n0 <- sum(y == 0)
    if (n1 == 0 || n0 == 0) return(NA_real_)
    ranks <- rank(p)
    (sum(ranks[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  r2_reg <- function(y, yhat) {
    ss_res <- sum((y - yhat)^2)
    ss_tot <- sum((y - mean(y))^2)
    1 - ss_res/ss_tot
  }

  # compute metrics
  if (metric == "auc") {
    in_m  <- auc_binary(y_in, in_mirror)
    in_nm <- auc_binary(y_in, in_nonmirror)
    ex_m  <- auc_binary(y_ext, ext_mirror)
    ex_nm <- auc_binary(y_ext, ext_nonmirror)
    metric_label <- "AUROC"
  } else if (metric == "r2") {
    in_m  <- r2_reg(y_in, in_mirror)
    in_nm <- r2_reg(y_in, in_nonmirror)
    ex_m  <- r2_reg(y_ext, ext_mirror)
    ex_nm <- r2_reg(y_ext, ext_nonmirror)
    metric_label <- "R2"
  } else {
    stop("metric must be 'auc' or 'r2'")
  }

  delta_in  <- in_m - in_nm
  delta_ext <- ex_m - ex_nm

  # risk flagging logic:
  # High if mirror >> non-mirror in-sample AND loses vs non-mirror on external
  risk_flag <- if (!is.na(delta_in) && delta_in >= delta_thresh && !is.na(delta_ext) && delta_ext <= 0) {
    "High (mirror inflation)"
  } else if (!is.na(delta_in) && abs(delta_in) < delta_thresh/2 && !is.na(delta_ext) && abs(delta_ext) < delta_thresh/2) {
    "Low"
  } else {
    "Medium"
  }

  data.frame(
    metric = metric_label,
    in_mirror = round(in_m, 3),
    in_nonmirror = round(in_nm, 3),
    delta_in = round(delta_in, 3),
    external_mirror = round(ex_m, 3),
    external_nonmirror = round(ex_nm, 3),
    delta_external = round(delta_ext, 3),
    risk_flag = risk_flag,
    stringsAsFactors = FALSE
  )
}
