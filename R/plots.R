#' Global theme for langAssessR figures
#' @param base_size base font size
#' @param base_family base font family
#' @export
theme_lang <- function(base_size = 11, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      plot.subtitle = ggplot2::element_text(color = "grey30"),
      plot.caption = ggplot2::element_text(color = "grey40", size = ggplot2::rel(0.9)),
      axis.title = ggplot2::element_text(face = "bold"),
      axis.text = ggplot2::element_text(color = "grey20"),
      panel.grid.major = ggplot2::element_line(color = "grey90"),
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(face = "bold")
    )
}

#' Color palette for langAssessR
#' @param n number of colors to return
#' @export
pal_lang <- function(n) {
  cols <- c("#1B9E77","#D95F02","#7570B3","#E7298A","#66A61E","#E6AB02","#A6761D","#1F78B4")
  cols[seq_len(n)]
}

#' Color scales for langAssessR
#' @param ... arguments passed to scale_color_manual/scale_fill_manual
#' @export
scale_color_lang <- function(...) ggplot2::scale_color_manual(values = pal_lang(8), ...)

#' @rdname scale_color_lang
#' @export
scale_fill_lang  <- function(...) ggplot2::scale_fill_manual(values = pal_lang(8), ...)

#' Plot site-wise performance
#' @param df data frame with site, estimate, lcl, ucl columns
#' @param metric_label label for y-axis
#' @export
plot_site_performance <- function(df, metric_label = "AUROC") {
  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(site, estimate), y = estimate)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lcl, ymax = ucl), color = "#1B9E77", size = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = metric_label, title = "Performance by site") +
    theme_lang()
}

#' Plot forest comparison
#' @param df data frame with site, estimate, model, lcl, ucl columns
#' @param metric_label label for x-axis
#' @export
plot_forest_compare <- function(df, metric_label = "AUROC") {
  ggplot2::ggplot(df, ggplot2::aes(y = stats::reorder(site, estimate), x = estimate, color = model)) +
    ggplot2::geom_vline(xintercept = 0.5, linetype = 3, color = "grey60") +
    ggplot2::geom_pointrange(ggplot2::aes(xmin = lcl, xmax = ucl), position = ggplot2::position_dodge(width = 0.6)) +
    scale_color_lang(name = NULL) +
    ggplot2::labs(y = NULL, x = metric_label, title = "Pooled CV vs. leave-site-out") +
    theme_lang()
}

#' Plot attention heatmap
#' @param df data frame with start and weight columns
#' @export
plot_attention_heatmap <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = start, y = 1, fill = weight)) +
    ggplot2::geom_tile(height = 0.9) +
    ggplot2::scale_fill_gradient(low = "white", high = "#E7298A", name = "Attention") +
    ggplot2::labs(x = "Position", y = NULL, title = "Attention over transcript") +
    theme_lang() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(), axis.ticks.y = ggplot2::element_blank())
}

#' Plot ablation
#' @param df data frame with window_id and delta columns
#' @export
plot_ablation <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(window_id, delta), y = delta)) +
    ggplot2::geom_col(fill = "#1B9E77") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = "Window", y = "Delta prediction", title = "Window-drop ablation") +
    theme_lang()
}

#' Plot topic lift
#' @param df data frame with topic, lift, and group columns
#' @export
plot_topic_lift <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(topic, lift), y = lift, fill = group)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.7)) +
    ggplot2::coord_flip() +
    scale_fill_lang(name = NULL) +
    ggplot2::labs(x = "Topic", y = "Lift", title = "Topic contrasts: TP vs FP") +
    theme_lang()
}

#' Plot AUC by group
#' @param df data frame with subgroup, auc, lcl, ucl columns
#' @export
plot_auc_by_group <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(subgroup, auc), y = auc)) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = lcl, ymax = ucl), color = "#7570B3", size = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "AUROC", title = "Performance by subgroup") +
    theme_lang()
}

#' Plot calibration by group
#' @param df data frame with mean_pred, obs_rate, subgroup columns
#' @export
plot_calibration_by_group <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = mean_pred, y = obs_rate, color = subgroup)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = 3, color = "grey60") +
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(size = 1.8) +
    scale_color_lang(name = NULL) +
    ggplot2::scale_x_continuous(labels = scales::percent) +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(x = "Predicted probability", y = "Observed rate", title = "Calibration by subgroup") +
    theme_lang()
}

#' Plot error balance
#' @param df data frame with subgroup, fnr, fpr columns
#' @export
plot_error_balance <- function(df) {
  df$balance <- df$fnr - df$fpr
  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(subgroup, balance), y = balance, fill = balance > 0)) +
    ggplot2::geom_col() +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = c("#D95F02","#1B9E77"), guide = "none") +
    ggplot2::labs(x = NULL, y = "FNR - FPR", title = "Error balance by subgroup") +
    theme_lang()
}

#' Plot decision curve
#' @param df data frame with threshold, net_benefit, policy columns
#' @export
plot_decision_curve <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = threshold, y = net_benefit, color = policy)) +
    ggplot2::geom_line(size = 1) +
    scale_color_lang(name = NULL) +
    ggplot2::labs(x = "Threshold probability", y = "Net benefit", title = "Decision-curve analysis") +
    theme_lang()
}

#' Plot de-identification counts
#' @param df data frame with entity and count columns
#' @export
plot_deid_counts <- function(df) {
  ggplot2::ggplot(df, ggplot2::aes(x = stats::reorder(entity, count), y = count)) +
    ggplot2::geom_col(fill = "#66A61E") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Count", title = "De-identified entities") +
    theme_lang()
}
