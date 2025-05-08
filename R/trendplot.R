library(dplyr)
library(ggplot2)
library(purrr)
library(stringr)
library(boot)

#' Plot Longitudinal Summary Statistics with Optional Smoothing and Confidence Intervals
#'
#' This function generates a plot showing central tendency (mean, median, or geometric mean)
#' and associated confidence intervals for longitudinal data, with optional features such as
#' individual-level data display, time binning, LOESS smoothing, and a count panel showing
#' the number of observations per time point.
#'
#' @param data A data frame containing the dataset to be visualized.
#' @param time_var Name of the column representing time (default = "Time").
#' @param conc_var Name of the column representing the observation variable (default = "conc").
#' @param id_var Name of the column representing subject ID (default = "Subject").
#' @param group_var Optional column name used to group data (e.g., treatment group).
#' @param show_individual Logical; if TRUE, show individual-level observations (default = FALSE).
#' @param connect_individual Logical; if TRUE, connect individual observations per subject (default = FALSE).
#' @param connect_central Logical; if TRUE, connect central tendency points by lines (default = FALSE).
#' @param show_counts Logical; if TRUE, show number of observations per time point in a bar plot below (default = FALSE).
#' @param x_lab Custom x-axis label. If NULL, defaults to "Time (h, binned if needed)".
#' @param y_lab Custom y-axis label. If NULL, inferred based on summary method used.
#' @param use_log_normal Logical; if TRUE, compute geometric mean (log-normal assumption, default = TRUE).
#' @param use_median Logical; if TRUE, compute median and interquartile range (default = FALSE).
#' @param iqr_range Numeric vector of length 2; quantile range for interval (default = c(0.25, 0.75)).
#' @param bootstrap_ci Logical; if TRUE, compute confidence intervals via bootstrapping (default = FALSE).
#' @param n_boot Number of bootstrap samples if bootstrap_ci is TRUE (default = 1000).
#' @param time_tolerance Numeric threshold for time binning trigger (default = 0.1).
#' @param bin_width Width of bins for time when binning is used (default = 0.5).
#' @param smooth Logical; if TRUE, overlay smoothed trend line (default = FALSE).
#' @param smooth_method Smoothing method for LOESS or GAM (default = "loess").
#' @param smooth_se Logical; if TRUE, include shaded confidence interval for smoothing (default = FALSE).
#'
#' @return A `ggplot` or `patchwork` object.
#' @export
#'
#' @examples
#' library(datasets)
#' data(Theoph)
#'
#' # Basic geometric mean plot
#' trendplot(Theoph)
#'
#' # Arithmetic mean with normal-theory CI
#' trendplot(Theoph, use_log_normal = FALSE)
#'
#' # Median with IQR
#' trendplot(Theoph, use_log_normal = FALSE, use_median = TRUE)
#'
#' # Median with bootstrapped CI
#' trendplot(Theoph, use_log_normal = FALSE, use_median = TRUE, bootstrap_ci = TRUE, n_boot = 200)
#'
#' # Individual data with connections
#' trendplot(Theoph, show_individual = TRUE, connect_individual = TRUE)
#'
#' # Smoothed geometric mean
#' trendplot(Theoph, smooth = TRUE)
#'
#' # Include count panel
#' trendplot(Theoph, show_counts = TRUE)
#'
#' # Custom labels
#' trendplot(Theoph, x_lab = "Nominal Time (hr)", y_lab = "Drug Concentration (mg/L)")
#'
#' # Grouped example with sleepstudy (lme4 package required)
#' if (requireNamespace("lme4", quietly = TRUE)) {
#'   library(lme4)
#'   data(sleepstudy)
#'   trendplot(
#'     sleepstudy,
#'     time_var = "Days",
#'     conc_var = "Reaction",
#'     id_var = "Subject",
#'     show_individual = TRUE,
#'     connect_individual = TRUE,
#'     use_log_normal = FALSE,
#'     connect_central = TRUE,
#'     smooth = TRUE,
#'     smooth_se = TRUE
#'   )
#' }

trendplot <- function(
    data,
    time_var = "Time",
    conc_var = "conc",
    id_var = "Subject",
    group_var = NULL,

    # Plot appearance and layout
    show_individual = FALSE,
    connect_individual = FALSE,
    connect_central = FALSE,
    show_counts = FALSE,
    x_lab = NULL,
    y_lab = NULL,

    # Summary method controls
    use_log_normal = TRUE,
    use_median = FALSE,
    iqr_range = c(0.25, 0.75),
    bootstrap_ci = FALSE,
    n_boot = 1000,

    # Time binning controls
    time_tolerance = 0.1,
    bin_width = 0.5,

    # Smoothing options
    smooth = FALSE,
    smooth_method = "loess",
    smooth_se = FALSE
) {

  # Validate inputs
  if (!is.numeric(iqr_range) || length(iqr_range) != 2 || any(iqr_range < 0 | iqr_range > 1)) {
    stop("iqr_range must be a numeric vector of length 2 with values between 0 and 1.")
  }
  if (!is.numeric(data[[time_var]])) stop("The time variable must be numeric.")
  if (!is.numeric(data[[conc_var]])) stop("The observation variable must be numeric.")
  if (!is.data.frame(data)) stop("'data' must be a data frame.")
  required_vars <- c(time_var, conc_var, id_var)
  if (!all(required_vars %in% names(data))) stop("Missing required variables in the dataset.")
  if (!is.null(group_var) && !group_var %in% names(data)) stop("'group_var' not found in the dataset.")
  if (use_log_normal && use_median) stop("Options 'use_log_normal' and 'use_median' cannot both be TRUE.")

  time_vals <- data[[time_var]]
  conc_vals <- data[[conc_var]]

  # Check if binning is needed
  should_bin <- any(diff(sort(unique(time_vals))) > time_tolerance)
  if (should_bin) message("Binning applied due to time variation > tolerance")

  if (should_bin) {
    data_binned <- data %>%
      mutate(
        Time_bin = cut(.data[[time_var]], breaks = seq(0, max(time_vals, na.rm = TRUE) + bin_width, by = bin_width), include.lowest = TRUE),
        Time_mid = map_dbl(Time_bin, ~ {
          bounds <- str_extract_all(.x, "[0-9.]+")[[1]]
          median(as.numeric(bounds))
        })
      )
  } else {
    data_binned <- data %>%
      mutate(Time_mid = .data[[time_var]])
  }

  # Summarize using selected method
  group_by_vars <- c("Time_mid")
  if (!is.null(group_var)) group_by_vars <- c(group_by_vars, group_var)

  # Bootstrap helper
  bootstrap_ci_fn <- function(vals, stat_fn, n = n_boot) {
    b <- boot::boot(vals, statistic = function(d, i) stat_fn(d[i]), R = n)
    quantile(b$t, probs = c(0.025, 0.975), na.rm = TRUE)
  }

  if (use_median) {
    summary_df <- data_binned %>%
      group_by(across(all_of(group_by_vars))) %>%
      summarise(
        n = sum(!is.na(.data[[conc_var]])),
        central_val = median(.data[[conc_var]], na.rm = TRUE),
        ci = list(if (bootstrap_ci) bootstrap_ci_fn(na.omit(.data[[conc_var]]), median) else quantile(.data[[conc_var]], probs = iqr_range, na.rm = TRUE)),
        .groups = "drop"
      ) %>%
      mutate(lower_CI = map_dbl(ci, 1), upper_CI = map_dbl(ci, 2))
    y_label <- "Observation (median with CI)"
    y_scale <- NULL
  } else if (use_log_normal) {
    data_binned <- data_binned %>% mutate(safe_conc = ifelse(.data[[conc_var]] <= 0, NA, .data[[conc_var]]))
    summary_df <- data_binned %>%
      group_by(across(all_of(group_by_vars))) %>%
      summarise(
        n = sum(!is.na(safe_conc)),
        log_vals = list(log(na.omit(safe_conc))),
        central_val = exp(mean(unlist(log_vals))),
        ci = list(if (bootstrap_ci) exp(bootstrap_ci_fn(unlist(log_vals), mean)) else exp(mean(unlist(log_vals)) + c(-1.96, 1.96) * sd(unlist(log_vals)) / sqrt(length(unlist(log_vals))))),
        .groups = "drop"
      ) %>%
      mutate(lower_CI = map_dbl(ci, 1), upper_CI = map_dbl(ci, 2))
    y_label <- "Observation (geometric mean)"
    y_scale <- scale_y_log10()
  } else {
    summary_df <- data_binned %>%
      group_by(across(all_of(group_by_vars))) %>%
      summarise(
        n = sum(!is.na(.data[[conc_var]])),
        vals = list(na.omit(.data[[conc_var]])),
        central_val = mean(unlist(vals)),
        ci = list(if (bootstrap_ci) bootstrap_ci_fn(unlist(vals), mean) else mean(unlist(vals)) + c(-1.96, 1.96) * sd(unlist(vals)) / sqrt(length(unlist(vals)))),
        .groups = "drop"
      ) %>%
      mutate(lower_CI = map_dbl(ci, 1), upper_CI = map_dbl(ci, 2))
    y_label <- "Observation (arithmetic mean)"
    y_scale <- NULL
  }

  # Base plot
  p <- ggplot()

  # Add individual data if requested
  if (show_individual) {
    p <- p + geom_point(data = data_binned, aes(x = .data[["Time_mid"]], y = .data[[conc_var]]), alpha = 0.4, color = "steelblue")
    if (connect_individual) {
      p <- p + geom_line(data = data_binned, aes(x = .data[["Time_mid"]], y = .data[[conc_var]], group = .data[[id_var]]), alpha = 0.2, color = "steelblue")
    }
  }

  # Add summary and CI
  p <- p +
    geom_point(data = summary_df, aes(x = Time_mid, y = central_val, color = if (!is.null(group_var)) .data[[group_var]] else NULL)) +
    geom_errorbar(data = summary_df, aes(x = Time_mid, ymin = lower_CI, ymax = upper_CI, color = if (!is.null(group_var)) .data[[group_var]] else NULL), width = 0.2)

  # Optionally connect summary points
  if (connect_central) {
    p <- p + geom_line(data = summary_df, aes(x = Time_mid, y = central_val, color = if (!is.null(group_var)) .data[[group_var]] else NULL))
  }

  # Add smoothing if requested
  if (smooth) {
    p <- p + geom_smooth(data = summary_df, aes(x = Time_mid, y = central_val, color = if (!is.null(group_var)) .data[[group_var]] else NULL), method = smooth_method, se = smooth_se, linetype = "dashed")
  }

  caption_lines <- c(
    if (use_median) {
      if (bootstrap_ci) {
        "Median observations with 95% CI from bootstrap resampling."
      } else {
        sprintf("Median observations with interquantile range (%.0fth–%.0fth percentile).", iqr_range[1]*100, iqr_range[2]*100)
      }
    } else if (use_log_normal) {
      if (bootstrap_ci) {
        "Geometric mean observations with 95% bootstrap CI, assuming log-normal distribution."
      } else {
        "Geometric mean observations with 95% CI (log-scale transformed)."
      }
    } else {
      if (bootstrap_ci) {
        "Arithmetic mean observations with 95% CI from bootstrap resampling."
      } else {
        "Arithmetic mean observations with 95% CI (±1.96×SE)."
      }
    },
    if (smooth) {
      if (smooth_se) {
        "LOESS smoothing line with CI overlay shown (dashed line)."
      } else {
        "LOESS smoothing line shown (dashed line)."
      }
    } else NULL
  )
  caption_text <- paste(caption_lines, collapse = "\n")

  p <- p +
    labs(caption = caption_text,
         x = if (is.null(x_lab)) "Time (h, binned if needed)" else x_lab,
         y = if (is.null(y_lab)) y_label else y_lab) +
    theme_minimal()

  if (!is.null(group_var)) {
    p <- p + labs(color = group_var)
  }

  if (!is.null(y_scale)) {
    p <- p + y_scale
  }

  if (show_counts && !is.null(summary_df$n)) {
    if (!requireNamespace("patchwork", quietly = TRUE)) {
      stop("Package 'patchwork' is required for show_counts = TRUE. Please install it.")
    }
    count_layer <- ggplot(summary_df, aes(x = Time_mid, y = n)) +
      geom_col(fill = "gray80", width = 0.3) +
      labs(x = NULL, y = "n", title = NULL) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        plot.margin = margin(5, 5, 5, 5)
      )

    p <- patchwork::wrap_plots(count_layer, p, ncol = 1, heights = c(0.2, 1))
  }

  return(p)
}

# ============================
# Example Usage
# # ============================
#
# # Load example data
# library(datasets)
# data(Theoph)
#
# # Example 1: Basic geometric mean plot
# trendplot(Theoph)
#
# # Example 2: Show individual observations and connect lines
# trendplot(Theoph, show_individual = TRUE, connect_individual = TRUE)
#
# # Example 3: Arithmetic mean with standard 95% CI
# plot_geom_mean_pk(Theoph, use_log_normal = FALSE)
#
# # Example 4: Median with IQR
# plot_geom_mean_pk(Theoph, use_log_normal = FALSE, use_median = TRUE)
#
# # Example 5: Median with bootstrapped 95% CI
# plot_geom_mean_pk(Theoph, use_log_normal = FALSE, use_median = TRUE, bootstrap_ci = TRUE, n_boot = 500)
#
# # Example 6: Apply smoothing
#
# # Example 6b: Show count layer below the main plot
# plot_geom_mean_pk(Theoph, show_individual = TRUE, show_counts = TRUE)
#
# # Example 7: Use sleepstudy dataset from lme4
# if (requireNamespace("lme4", quietly = TRUE)) {
#   library(lme4)
#   data(sleepstudy)
#
#   # Use arithmetic mean and connect individual lines
#   trendplot(
#     sleepstudy,
#     time_var = "Days",
#     conc_var = "Reaction",
#     id_var = "Subject",
#     show_individual = TRUE,
#     connect_individual = TRUE,
#     use_log_normal = FALSE,
#     connect_central = TRUE,
#     smooth = TRUE,
#     smooth_se = TRUE
#   )
# }
# plot_geom_mean_pk(Theoph, smooth = TRUE, smooth_method = "loess", smooth_se = TRUE)

