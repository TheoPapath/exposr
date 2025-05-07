# Stable version


# Currently Implemented
# Two-panel structure with exposure-response (top) and exposure vs. dose (bottom).
# Observed data display, optionally (show_points).
# Group-based coloring, configurable by group_var.
# Binning and summary by exposure bins (median/mean/geometric mean).
# External predictions overlay with ribbons.
# Trend line options (loess, lm, etc.) and CI toggling.
# Dose-level exposure annotations
# Geometric mean (diamond)
# Dose label
# Y-axis anchoring that respects ylims.
# Optional vertical median lines and EC50/EC90 crosshairs.
# Flexible axis labels, limits, log-scaling, and legends.
# Extensive captioning, dynamically adapting to plot contents.
# Test dataset with grouped emax-based response generation.
# Faceted annotations like effect-level lines positioned dynamically.

# To do
# Return data invisibly (optionally) for downstream use, e.g., return_data = FALSE.
# User control for 90% range percentiles (e.g., configurable range_quantiles = c(0.05, 0.95)).
# Optional x-axis annotation of bin boundaries (to show exposure bucket cutoffs).

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)  # for combining plots
library(Hmisc)      # for binning and CIs
library(rlang)      # for tidy evaluation

# Define generic color palette generator
get_generic_palette <- function(groups) {
  colors <- RColorBrewer::brewer.pal(max(3, length(groups)), "Set1")
  setNames(colors[1:length(groups)], groups)
}

#' Plot Exposure-Response Relationship
#'
#' Creates a two-panel ggplot visualization showing the relationship between dose, exposure, and response.
#'
#' @param data A data frame containing the dose, exposure, and response variables.
#' @param dose_var Unquoted name of the dose variable.
#' @param exposure_var Unquoted name of the exposure variable.
#' @param response_var Unquoted name of the response variable.
#' @param xlims Optional vector of length 2 specifying x-axis limits.
#' @param ylims Optional vector of length 2 specifying y-axis limits for the upper panel.
#' @param xlab Optional custom label for the x-axis (shared).
#' @param ylab_top Optional label for the y-axis of the top panel.
#' @param ylab_bottom Optional label for the y-axis of the bottom panel.
#' @param log_scale Character string: "none", "logx", "logy", or "logxy" to apply log scaling.
#' @param n_bins Number of exposure bins for summary statistics.
#' @param summary_type Summary function for exposure: "geometric", "median", or "mean".
#' @param add_trend Logical; whether to overlay a trend line.
#' @param trend_method Method for trend line: "loess", "lm", "gam", or "glm".
#' @param trend_ci Logical; whether to include a confidence interval ribbon around the trend.
#' @param trend_ci_level Confidence level for the trend CI.
#' @param group_var Optional unquoted grouping variable for color separation.
#' @param external_df Optional data frame with predicted exposure-response values to overlay.
#' @param external_mapping A list of column names in external_df: list(x = ..., y = ..., lower = ..., upper = ..., group = ...).
#' @param show_effect_lines Character; "none", "eff50", "eff90", or "both" to indicate which effect threshold lines to show.
#' @param vertical_line Optional numeric value where a vertical dashed line (e.g., EC50/EC90) will be drawn in both panels.
#'
#' @return A ggplot object with two panels: response vs exposure, and exposure vs dose.
#' @export

plot_exposure_response <- function(data, dose_var, exposure_var, response_var,
                                   show_median_lines = TRUE,
                                   show_points = FALSE,
                                   effect_label_position = c("left", "right"),
                                   show_effect_lines = "none",
                                   external_df = NULL, external_mapping = NULL,
                                   xlims = NULL, ylims = NULL,
                                   vertical_line = NULL,
                                   xlab = NULL, ylab_top = NULL, ylab_bottom = NULL,
                                   log_scale = c("none", "logx", "logy", "logxy"),
                                   n_bins = 6, summary_type = c("geometric", "median", "mean"),
                                   add_trend = TRUE, trend_method = c("loess", "lm", "gam", "glm"),
                                   trend_ci = TRUE, trend_ci_level = 0.95,
                                   group_var = NULL) {

  summary_type <- match.arg(summary_type)
  log_scale <- match.arg(log_scale)
  show_effect_lines <- match.arg(show_effect_lines, choices = c("none", "eff50", "eff90", "both"))
  effect_label_position <- match.arg(effect_label_position)
  trend_method <- match.arg(trend_method)

  # Tidy evaluation for grouping variable
  dose_var <- enquo(dose_var)
  exposure_var <- enquo(exposure_var)
  response_var <- enquo(response_var)
  group_var <- enquo(group_var)

  data <- data %>% mutate(Dose = !!dose_var, Exposure = !!exposure_var, Response = !!response_var)

  # Remove rows with Exposure <= 0 if log scaling on x-axis is requested
  if (log_scale %in% c("logx", "logxy")) {
    zero_count <- sum(data$Exposure <= 0, na.rm = TRUE)
    if (zero_count > 0) {
      warning(sprintf("%d row(s) with Exposure <= 0 removed due to log scale on x-axis.", zero_count))
      data <- data %>% filter(Exposure > 0)
    }
  }

  if (!rlang::quo_is_null(group_var)) {
    data <- data %>% mutate(Group = !!group_var)
  } else {
    data <- data %>% mutate(Group = "All")
  }

  x_limits <- if (is.null(xlims)) range(data$Exposure, na.rm = TRUE) else xlims

  binning_data <- data %>% filter(Dose > 0)
  binning_data <- binning_data %>%
    group_by(Group) %>%
    mutate(Exposure_bin = cut2(Exposure, g = n_bins)) %>%
    ungroup()

  summary_df <- binning_data %>%
    group_by(Group, Exposure_bin) %>%
    summarise(
      Exposure_stat = case_when(
        summary_type == "geometric" ~ exp(mean(log(Exposure))),
        summary_type == "median" ~ median(Exposure),
        TRUE ~ mean(Exposure)
      ),
      Response_stat = mean(Response),
      Response_se = sd(Response) / sqrt(n()),
      Response_lower = Response_stat - 1.96 * Response_se,
      Response_upper = Response_stat + 1.96 * Response_se,
      .groups = "drop"
    )

  median_exposures <- data %>%
    filter(Dose > 0) %>%
    group_by(Dose) %>%
    summarise(med_exp = median(Exposure), .groups = "drop")

  caption_text <- case_when(
    summary_type == "geometric" ~ "Points represent geometric means of exposure bins with 95% confidence intervals.",
    summary_type == "median" ~ "Points represent medians of exposure bins with 95% confidence intervals around the mean response.",
    summary_type == "mean" ~ "Points represent mean exposure and mean response with 95% confidence intervals."
  )
  caption_text <- paste(caption_text,
                        "Grey horizontal lines with diamonds along the bottom of the top panel represent geometric means and 90% exposure ranges at each dose level.",
                        sep = "
")

  if (show_points) {
    caption_text <- paste(caption_text, "Transparent points represent individual observed data.", sep = "
")
  }

  if (add_trend && trend_method %in% c("loess", "lm", "gam", "glm")) {
    method_desc <- case_when(
      trend_method == "loess" ~ "A LOESS trend line is overlaid using the raw exposure-response data.",
      trend_method == "lm" ~ "A linear regression trend line is overlaid using the raw exposure-response data.",
      trend_method == "gam" ~ "A GAM smooth trend line is overlaid using the raw exposure-response data.",
      trend_method == "glm" ~ "A generalized linear model trend line is overlaid using the raw exposure-response data."
    )
    caption_text <- paste(caption_text, method_desc, sep = "\n")
    if (trend_ci) {
      caption_text <- paste(caption_text, paste0("The trend line includes a ", trend_ci_level * 100, "% confidence interval."), sep = "\n")
    }
  }

  # Upper panel
  exposure_summary_by_dose <- data %>%
    filter(Dose > 0) %>%
    group_by(Dose) %>%
    summarise(
      exp_gm = exp(mean(log(Exposure), na.rm = TRUE)),
      exp_min = quantile(Exposure, 0.05, na.rm = TRUE),
      exp_max = quantile(Exposure, 0.95, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(y = (if (!is.null(ylims)) ylims[1] else min(summary_df$Response_stat, na.rm = TRUE)) +
             0.02 * diff(if (!is.null(ylims)) ylims else range(summary_df$Response_stat, na.rm = TRUE)) * (rev(rank(Dose)) - 1))

  p1 <- ggplot()

  if (show_points) {
    p1 <- p1 +
      geom_point(data = data, aes(x = Exposure, y = Response, color = Group), alpha = 0.3, shape = 16, size = 1)
  }

  p1 <- p1 +
    geom_point(data = summary_df, aes(x = Exposure_stat, y = Response_stat, color = Group), size = 2) +
    geom_errorbar(data = summary_df, aes(x = Exposure_stat, ymin = Response_lower, ymax = Response_upper, color = Group), width = 0)

  if (add_trend && trend_method %in% c("loess", "lm", "gam", "glm")) {
    p1 <- p1 + geom_smooth(data = data, aes(x = Exposure, y = Response, color = Group),
                           method = trend_method, formula = y ~ x, se = trend_ci, level = trend_ci_level)
  }

  if (log_scale %in% c("logx", "logxy")) p1 <- p1 + scale_x_log10()
  if (log_scale %in% c("logy", "logxy")) p1 <- p1 + scale_y_log10()

  p1 <- p1 +
    scale_color_manual(values = get_generic_palette(unique(data$Group))) +
    { if (show_median_lines) geom_vline(data = median_exposures, aes(xintercept = med_exp), linetype = "dotted", color = "grey30") else NULL } +
    { if (!is.null(vertical_line)) geom_vline(xintercept = vertical_line, linetype = "dashed", color = "black") else NULL } +
    coord_cartesian(xlim = x_limits, ylim = ylims) +
    labs(x = ifelse(is.null(xlab), paste0("Exposure (", tools::toTitleCase(summary_type), ")"), xlab),
         y = ifelse(is.null(ylab_top), "Response", ylab_top)) +
    theme_minimal() +
    geom_segment(data = exposure_summary_by_dose, aes(x = exp_min, xend = exp_max, y = y, yend = y), linewidth = 1.5, color = "grey", lineend = "round") +
    geom_point(data = exposure_summary_by_dose, aes(x = exp_gm, y = y), shape = 18, color = "black", size = 2) +
    geom_text(data = exposure_summary_by_dose, aes(x = exp_max, y = y + 0.03 * diff(if (!is.null(ylims)) ylims else range(summary_df$Response_stat, na.rm = TRUE)), label = Dose), hjust = 1, size = 3)

  # Add external prediction data if provided
  if (!is.null(external_df) && !is.null(external_mapping)) {
    x_ext <- external_mapping$x
    y_ext <- external_mapping$y
    lower_ext <- external_mapping$lower
    upper_ext <- external_mapping$upper
    group_ext <- external_mapping$group

    p1 <- p1 +
      geom_line(data = external_df, aes_string(x = x_ext, y = y_ext, color = group_ext), show.legend = FALSE)

    if (!is.null(lower_ext) && !is.null(upper_ext)) {
      p1 <- p1 +
        geom_ribbon(data = external_df,
                    aes_string(x = x_ext, ymin = lower_ext, ymax = upper_ext, fill = group_ext, group = group_ext),
                    alpha = 0.2, inherit.aes = FALSE)
    }
  }

  # Lower panel
  p2 <- data %>% filter(Dose > 0) %>%
    ggplot(aes(x = Exposure, y = factor(Dose))) +
    geom_boxplot(fill = "white", outlier.shape = NA) +
    { if (show_median_lines) geom_vline(data = median_exposures, aes(xintercept = med_exp), linetype = "dotted", color = "grey30") else NULL } +
    { if (!is.null(vertical_line)) geom_vline(xintercept = vertical_line, linetype = "dashed", color = "black") else NULL } +
    coord_cartesian(xlim = x_limits) +
    labs(x = ifelse(is.null(xlab), "Exposure", xlab),
         y = ifelse(is.null(ylab_bottom), "Dose", ylab_bottom)) +
    theme_minimal()

  if (log_scale %in% c("logx", "logxy")) p2 <- p2 + scale_x_log10()

  # Optional effect level crosshairs
  if (show_effect_lines != "none") {
    effect_annotation_text <- c()
    effect_lines_data <- summary_df

    if (show_effect_lines %in% c("eff50", "both")) {
      med_eff50 <- effect_lines_data %>% group_by(Group) %>% summarise(
        y = median(Response_stat, na.rm = TRUE),
        x = Exposure_stat[which.min(abs(Response_stat - median(Response_stat, na.rm = TRUE)))], .groups = "drop")

      p1 <- p1 +
        geom_hline(data = med_eff50, aes(yintercept = y, color = Group), linetype = "dashed", show.legend = FALSE) +
        geom_vline(data = med_eff50, aes(xintercept = x, color = Group), linetype = "dashed", show.legend = FALSE) +
        geom_text(data = med_eff50, aes(x = ifelse(effect_label_position == "left", min(x_limits), max(x_limits)), y = y, label = "50% effect", color = Group), hjust = ifelse(effect_label_position == "left", -0.1, 1.1), vjust = -0.5, show.legend = FALSE)

      effect_annotation_text <- c(effect_annotation_text, "Dashed crosshairs mark the approximate 50% effect level per group.")
    }

    if (show_effect_lines %in% c("eff90", "both")) {
      med_eff90 <- effect_lines_data %>% group_by(Group) %>% summarise(
        y = quantile(Response_stat, 0.9, na.rm = TRUE),
        x = Exposure_stat[which.min(abs(Response_stat - quantile(Response_stat, 0.9, na.rm = TRUE)))], .groups = "drop")

      p1 <- p1 +
        geom_hline(data = med_eff90, aes(yintercept = y, color = Group), linetype = "dashed", show.legend = FALSE) +
        geom_vline(data = med_eff90, aes(xintercept = x, color = Group), linetype = "dashed", show.legend = FALSE) +
        geom_text(data = med_eff90, aes(x = ifelse(effect_label_position == "left", min(x_limits), max(x_limits)), y = y, label = "90% effect", color = Group), hjust = ifelse(effect_label_position == "left", -0.1, 1.1), vjust = -0.5, show.legend = FALSE)

      effect_annotation_text <- c(effect_annotation_text, "Dashed crosshairs mark the approximate 90% effect level per group.")
    }

    caption_text <- paste(caption_text, paste(effect_annotation_text, collapse = "
"), sep = "
")
  }

  # Combine panels
  final_plot <- (p1 / p2 + plot_layout(heights = c(4, 1))) &
    plot_annotation(caption = paste(
      caption_text,
      if (show_median_lines) {
        if (nrow(median_exposures) > 1)
          "Vertical dotted lines indicate the median exposure observed for each dose level."
        else
          "A vertical dotted line indicates the median exposure observed for the single dose level."
      } else NULL,
      sep = "\n"
    ))

  return(final_plot)
}


# Examples

# # Simulate example data for testing
# set.seed(123)
# n_placebo_per_trial <- 30
# n_active_per_trial <- 120
#
# df_trialA <- data.frame(
#   Dose = c(rep(0, n_placebo_per_trial), rep(c(50, 100, 300, 600), each = n_active_per_trial / 4)),
#   Trial = "Trial A"
# )
#
# df_trialB <- data.frame(
#   Dose = c(rep(0, n_placebo_per_trial), rep(c(50, 100, 300, 600), each = n_active_per_trial / 4)),
#   Trial = "Trial B"
# )
#
# df_demo <- bind_rows(df_trialA, df_trialB)
# n <- nrow(df_demo)
#
# clearance <- ifelse(df_demo$Trial == "Trial A", 5, 6)
# rate <- 1 / (df_demo$Dose / clearance)
# rate[df_demo$Dose == 0] <- NA
# exposure_vals <- rexp(n, rate = rate)
# df_demo$Exposure <- ifelse(df_demo$Dose == 0, 0, exposure_vals)
#
# effect_shift <- ifelse(df_demo$Trial == "Trial A", 0, 1.5)
# df_demo$Response <- ifelse(df_demo$Dose == 0,
#                            8 + effect_shift + rnorm(n, 0, 0.5),
#                            8.5 + effect_shift + (7 * df_demo$Exposure / (50 + df_demo$Exposure)) + rnorm(n, 0, 0.5))
#
# # Example external prediction data using fitted Emax model
# library(minpack.lm)
#
# # Fit Emax model by trial and generate predictions
# external_pred <- df_demo %>%
#   filter(Dose > 0) %>%
#   group_by(Trial) %>%
#   group_modify(~{
#     df <- .x
#     fit <- try(nlsLM(Response ~ E0 + Emax * Exposure / (EC50 + Exposure),
#                      data = df,
#                      start = list(E0 = 7, Emax = 6, EC50 = 50)), silent = TRUE)
#     if (inherits(fit, "try-error")) return(NULL)
#     pred_grid <- data.frame(Exposure = seq(0, max(df$Exposure), length.out = 100))
#     preds <- predict(fit, newdata = pred_grid)
#     pred_grid$Response <- preds
#     pred_grid$Lower <- preds - 0.5
#     pred_grid$Upper <- preds + 0.5
#     pred_grid$Trial <- df$Trial[1]
#     pred_grid
#   }) %>%
#   ungroup()
#
# # Example call
# plot_exposure_response(
#   df_demo, Dose, Exposure, Response,
#   n_bins = 6, ylims = c(5,20),
#   # vertical_line = 250,
#   summary_type = "geometric",
#   add_trend = FALSE,
#   # trend_method = "loess",
#   trend_ci = TRUE,
#   trend_ci_level = 0.95,
#   group_var = Trial,
#   external_df = external_pred,
#   external_mapping = list(x = "Exposure", y = "Response", lower = "Lower", upper = "Upper", group = "Trial"),
#   log_scale = "none", show_effect_lines = "eff90", effect_label_position = "right"
# )

