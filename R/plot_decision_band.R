#' Plot Decision Band
#'
#' Creates a decision band plot showing exposure-response data with overlaid efficacy
#' and safety thresholds. Useful for visualizing the range of exposures where efficacy
#' is acceptable and safety is not compromised.
#'
#' @param data A data frame containing exposure-response data.
#' @param exposure_var Column name for exposure (x-axis).
#' @param response_var Column name for predicted or observed response (y-axis).
#' @param group_var Optional column used for coloring points (e.g., treatment group).
#' @param efficacy_thresh Numeric or vector; efficacy threshold(s).
#' @param safety_thresh Numeric or vector; safety threshold(s).
#' @param dose_lines Optional numeric vector of reference vertical lines (e.g., studied doses).
#' @param smooth Logical; if TRUE, overlay a smoothed trend line.
#' @param smooth_method Character; smoothing method for trend line (default = 'loess').
#' @param ci Logical; if TRUE, show a confidence ribbon.
#' @param bin_exposure Logical; if TRUE, bin exposure into quantiles.
#' @param n_bins Integer; number of bins when binning exposure.
#' @param band_fill Fill color for the decision band.
#' @param band_alpha Alpha level (transparency) for decision band.
#' @param title Plot title.
#' @param x_lab Label for the x-axis.
#' @param y_lab Label for the y-axis.
#' @param predicted_var Optional column name for predicted values to overlay.
#' @param facet_var Optional column name for faceting by subgroup.
#' @param response_trans Optional transformation function for response (e.g., 'log', 'logit').
#' @param log_x Logical; if TRUE, use log10 scale for x-axis.
#' @param legend_title Optional character string for the legend title.
#' @param highlight_doses Optional list of lists to highlight vertical dose regions (e.g., list(list(min=100, max=200))).
#'
#' @return A ggplot object.
#' @export
#'
#' @examples
#' set.seed(123)
#' df <- data.frame(
#'   exposure = seq(1, 1000, length.out = 100),
#'   response = 100 / (1 + exp(-0.005 * (seq(0, 1000, length.out = 100) - 500))) + rnorm(100, 0, 5),
#'   group = rep(c("A", "B"), each = 50),
#'   pred = 100 / (1 + exp(-0.005 * (seq(0, 1000, length.out = 100) - 500)))
#' )
#' plot_decision_band(
#'   df,
#'   exposure_var = "exposure",
#'   response_var = "response",
#'   predicted_var = "pred",
#'   group_var = "group",
#'   efficacy_thresh = 60,
#'   safety_thresh = 90,
#'   dose_lines = c(100, 300, 600),
#'   log_x = TRUE,
#'   facet_var = "group",
#'   response_trans = NULL,
#'   bin_exposure = TRUE,
#'   band_fill = "lightblue",
#'   legend_title = "Group"
#' )
#' df <- data.frame(
#'   exposure = seq(0, 1000, length.out = 100),
#'   response = 100 / (1 + exp(-0.005 * (seq(0, 1000, length.out = 100) - 500))) + rnorm(100, 0, 5)
#' )
#' plot_decision_band(
#'   df,
#'   exposure_var = "exposure",
#'   response_var = "response",
#'   efficacy_thresh = 60,
#'   safety_thresh = 90,
#'   dose_lines = c(100, 300, 600)
#' )

plot_decision_band <- function(
    data,
    exposure_var,
    response_var,
    group_var = NULL,
    efficacy_thresh,
    safety_thresh,
    dose_lines = NULL,
    smooth = TRUE,
    smooth_method = "loess",
    ci = TRUE,
    bin_exposure = FALSE,
    n_bins = 4,
    band_fill = "blue",
    band_alpha = 0.1,
    title = "Exposure–Response with Decision Bands",
    x_lab = "Exposure",
    y_lab = "Response"
    ,
    predicted_var = NULL,
    facet_var = NULL,
    response_trans = NULL,
    log_x = FALSE,
    legend_title = NULL,
    highlight_doses = NULL
) {
  library(ggplot2)
  library(dplyr)

  data <- data %>% mutate(
    Exposure = .data[[exposure_var]],
    Response = .data[[response_var]]
  )

  if (!is.null(predicted_var)) {
    data$Predicted <- data[[predicted_var]]
  }

  if (!is.null(response_trans)) {
    trans_fn <- match.fun(response_trans)
    data <- data %>% mutate(Response = trans_fn(Response))
    if (!is.null(predicted_var)) {
      data <- data %>% mutate(Predicted = trans_fn(Predicted))
    }
  }

  if (!is.null(group_var)) {
    data$Group <- as.factor(data[[group_var]])
  }

  if (bin_exposure) {
    data$ExposureBin <- cut(
      data$Exposure,
      breaks = quantile(data$Exposure, probs = seq(0, 1, length.out = n_bins + 1), na.rm = TRUE),
      include.lowest = TRUE
    )
  }

  p <- ggplot(data, aes(x = Exposure, y = Response))

  # Decision zone
  if (!is.null(highlight_doses)) {
    for (region in highlight_doses) {
      p <- p + annotate(
        "rect",
        xmin = region$min,
        xmax = region$max,
        ymin = -Inf,
        ymax = Inf,
        alpha = 0.1,
        fill = region$fill %||% "gray80"
      )
    }
  }

  p <- p + annotate(
    "rect",
    xmin = min(data$Exposure, na.rm = TRUE),
    xmax = max(data$Exposure, na.rm = TRUE),
    ymin = min(efficacy_thresh),
    ymax = max(safety_thresh),
    alpha = band_alpha,
    fill = band_fill
  )

  # Horizontal thresholds
  for (et in efficacy_thresh) {
    p <- p + geom_hline(yintercept = et, linetype = "dashed", color = "darkgreen")
  }
  for (st in safety_thresh) {
    p <- p + geom_hline(yintercept = st, linetype = "dashed", color = "red")
  }

  # Vertical dose lines
  if (!is.null(dose_lines)) {
    for (dl in dose_lines) {
      p <- p + geom_vline(xintercept = dl, linetype = "dotted", color = "gray40")
    }
  }

  # Points
  if (!is.null(group_var)) {
    p <- p + geom_point(aes(color = Group)) + scale_color_brewer(palette = "Set1")
  } else if (bin_exposure) {
    p <- p + geom_point(aes(color = ExposureBin)) + scale_color_brewer(palette = "Blues")
  } else {
    p <- p + geom_point(aes(color = Exposure)) + scale_color_gradient(low = "#deebf7", high = "#08519c")
  }

  # Smoothing
  if (smooth) {
    method_family <- if (length(unique(data$Response)) <= 2 && all(data$Response %in% c(0, 1))) "binomial" else "gaussian"
    p <- p + geom_smooth(method = smooth_method, se = ci, method.args = list(family = method_family), color = "black")
  }

  p <- p +
    theme_minimal() +
    scale_x_continuous(trans = if (log_x) "log10" else "identity", expand = expansion(mult = c(0.02, 0.02))) +
    labs(
      title = title,
      color = legend_title,
      x = x_lab,
      y = y_lab,
      caption = paste0(
        "Shaded band = decision region (", min(efficacy_thresh), "–", max(safety_thresh), "); ",
        "Green line = efficacy threshold; Red line = safety threshold"
      )
    ) +
    theme(
      plot.caption = element_text(size = 9, hjust = 0)
    ) +
    scale_x_continuous(expand = expansion(mult = c(0.02, 0.02)))

  if (!is.null(predicted_var)) {
    p <- p + geom_line(aes(y = Predicted), color = "black", linewidth = 1)
  }

  if (!is.null(facet_var) && facet_var %in% names(data)) {
    p <- p + facet_wrap(as.formula(paste("~", facet_var)), scales = "free_y")
  }

  return(p)
}
