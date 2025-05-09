#' Plot Benefit-Risk Grid
#'
#' Creates a 2D decision space plot showing efficacy vs safety probabilities or values.
#' Useful for dose selection or benefit-risk communication. Accepts group-wise data
#' and overlays thresholds to define acceptable decision regions.
#'
#' @param data A data frame containing one row per dose or scenario.
#' @param efficacy_var Column name for efficacy estimate or probability.
#' @param safety_var Column name for safety estimate or probability.
#' @param dose_var Optional column name for labeling dose levels.
#' @param efficacy_thresh Numeric; minimum acceptable efficacy.
#' @param safety_thresh Numeric; maximum acceptable safety risk.
#' @param label_points Logical; if TRUE, label points with dose.
#' @param title Title for the plot.
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis.
#' @param facet_var Optional variable to facet the grid by subgroup.
#' @param efficacy_lower Optional column for lower bound of efficacy CI.
#' @param efficacy_upper Optional column for upper bound of efficacy CI.
#' @param safety_lower Optional column for lower bound of safety CI.
#' @param safety_upper Optional column for upper bound of safety CI.
#' @param quadrant_colors Named list of fill colors for each benefit-risk quadrant.

#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   dose = c(10, 20, 40, 80, 160),
#'   prob_efficacy = c(0.2, 0.4, 0.6, 0.8, 0.9),
#'   prob_safety = c(0.05, 0.1, 0.2, 0.35, 0.6),
#'   subgroup = rep(c("A", "B"), length.out = 5),
#'   eff_lo = c(0.1, 0.3, 0.5, 0.7, 0.8),
#'   eff_hi = c(0.3, 0.5, 0.7, 0.9, 0.95),
#'   saf_lo = c(0.01, 0.05, 0.1, 0.25, 0.5),
#'   saf_hi = c(0.08, 0.12, 0.3, 0.45, 0.7)
#' )
#' plot_benefit_risk_grid(
#'   df,
#'   efficacy_var = "prob_efficacy",
#'   safety_var = "prob_safety",
#'   dose_var = "dose",
#'   efficacy_thresh = 0.6,
#'   safety_thresh = 0.3,
#'   efficacy_lower = "eff_lo",
#'   efficacy_upper = "eff_hi",
#'   safety_lower = "saf_lo",
#'   safety_upper = "saf_hi",
#'   facet_var = "subgroup"
#' )
#' df <- data.frame(
#'   dose = c(10, 20, 40, 80, 160),
#'   prob_efficacy = c(0.2, 0.4, 0.6, 0.8, 0.9),
#'   prob_safety = c(0.05, 0.1, 0.2, 0.35, 0.6)
#' )
#' plot_benefit_risk_grid(
#'   df,
#'   efficacy_var = "prob_efficacy",
#'   safety_var = "prob_safety",
#'   dose_var = "dose",
#'   efficacy_thresh = 0.6,
#'   safety_thresh = 0.3
#' )

plot_benefit_risk_grid <- function(
    data,
    efficacy_var,
    safety_var,
    dose_var = NULL,
    efficacy_thresh,
    safety_thresh,
    label_points = TRUE,
    facet_var = NULL,
    efficacy_lower = NULL,
    efficacy_upper = NULL,
    safety_lower = NULL,
    safety_upper = NULL,
    quadrant_colors = list(
      green = "#ccebc5",
      red = "#fbb4ae",
      amber_top = "#ffffcc",
      amber_right = "#d9f0ff"
    ),
    title = "Benefit–Risk Grid",
    x_lab = "Efficacy Probability",
    y_lab = "Safety Risk Probability"
) {
  library(ggplot2)
  library(dplyr)

  data <- data %>%
    mutate(
      Efficacy = .data[[efficacy_var]],
      Safety = .data[[safety_var]],
      DoseLabel = if (!is.null(dose_var)) as.character(.data[[dose_var]]) else NA
    )

  p <- ggplot(data, aes(x = Efficacy, y = Safety)) +
    annotate("rect", xmin = 0, xmax = efficacy_thresh, ymin = safety_thresh, ymax = 1, fill = quadrant_colors$red, alpha = 0.1) +
    annotate("rect", xmin = efficacy_thresh, xmax = 1, ymin = safety_thresh, ymax = 1, fill = quadrant_colors$amber_top, alpha = 0.1) +
    annotate("rect", xmin = 0, xmax = efficacy_thresh, ymin = 0, ymax = safety_thresh, fill = quadrant_colors$amber_right, alpha = 0.1) +
    annotate("rect", xmin = efficacy_thresh, xmax = 1, ymin = 0, ymax = safety_thresh, fill = quadrant_colors$green, alpha = 0.2) +
    geom_rect(
      xmin = efficacy_thresh,
      xmax = 1,
      ymin = 0,
      ymax = safety_thresh,
      fill = "lightgreen",
      alpha = 0.2
    ) +
    geom_hline(yintercept = safety_thresh, linetype = "dashed", color = "red") +
    geom_vline(xintercept = efficacy_thresh, linetype = "dashed", color = "darkgreen") +
    geom_point(size = 3, color = "steelblue") +
    {
      if (!is.null(efficacy_lower) && !is.null(efficacy_upper)) {
        geom_errorbarh(aes(xmin = .data[[efficacy_lower]], xmax = .data[[efficacy_upper]]), height = 0.02)
      } else NULL
    } +
    {
      if (!is.null(safety_lower) && !is.null(safety_upper)) {
        geom_errorbar(aes(ymin = .data[[safety_lower]], ymax = .data[[safety_upper]]), width = 0.02)
      } else NULL
    } +
    theme_minimal() +
    theme(panel.grid.minor = element_blank()) +
    labs(
      title = title,
      x = x_lab,
      y = y_lab,
      caption = paste0(
        "Green shaded region: Acceptable (efficacy >= ", efficacy_thresh,
        ", safety <= ", safety_thresh, ")"
      )
    )

  if (label_points && !is.null(dose_var)) {
    p <- p + geom_text(aes(label = DoseLabel), vjust = -0.6, size = 3.5)
  }

  if (!is.null(facet_var) && facet_var %in% names(data)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)))
  }

  return(p)
}
