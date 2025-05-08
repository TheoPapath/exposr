# Logistic regression variant of the exposure-response plot
#
# Supports binary outcomes and allows toggling between:
#   - Proportion of responders (bin-based)
#   - Model-predicted probabilities (logistic regression)

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

# Logistic exposure-response plotting function
plot_logistic_exposure_response <- function(
    data,
    dose_var,
    exposure_var,
    response_var,
    group_var = NULL,
    logx = FALSE,
    show_points = TRUE,
    bin_count = 5,
    summary_stat = c("mean", "median", "geomean"),
    y_scale = c("proportion", "predicted"),
    conf_level = 0.95,
    show_model_fit = TRUE,
    return_data = FALSE
) {
  summary_stat <- match.arg(summary_stat)
  y_scale <- match.arg(y_scale)
  data <- data %>% drop_na({{exposure_var}}, {{response_var}}, {{dose_var}})
  if (!is.null(group_var)) data <- data %>% drop_na({{group_var}})

  df <- data %>%
    mutate(
      exposure = !!sym(exposure_var),
      response = !!sym(response_var),
      dose = !!sym(dose_var),
      group = if (!is.null(group_var)) as.factor(!!sym(group_var)) else "All"
    )

  palette <- get_generic_palette(unique(df$group))

  df$bin <- cut2(df$exposure, g = bin_count)

  bin_summary <- df %>%
    group_by(group, bin) %>%
    summarise(
      x = case_when(
        summary_stat == "mean" ~ mean(exposure, na.rm = TRUE),
        summary_stat == "median" ~ median(exposure, na.rm = TRUE),
        summary_stat == "geomean" ~ exp(mean(log(exposure), na.rm = TRUE))
      ),
      y = mean(response, na.rm = TRUE),
      n = n(),
      se = sqrt(y * (1 - y) / n),
      ci_low = y - qnorm(1 - (1 - conf_level)/2) * se,
      ci_high = y + qnorm(1 - (1 - conf_level)/2) * se
    ) %>% ungroup()

  model_fit <- NULL
  if (show_model_fit && y_scale == "predicted") {
    model_fit <- glm(response ~ exposure, data = df, family = binomial)
    pred_data <- data.frame(exposure = seq(min(df$exposure), max(df$exposure), length.out = 100))
    pred_data$pred <- predict(model_fit, newdata = pred_data, type = "response")
  }

  p1 <- ggplot() +
    theme_bw() +
    labs(x = "Exposure", y = "Response")

  if (show_points) {
    p1 <- p1 + geom_jitter(data = df, aes(x = exposure, y = response, color = group), height = 0.05, alpha = 0.4)
  }

  if (y_scale == "proportion") {
    p1 <- p1 +
      geom_point(data = bin_summary, aes(x = x, y = y, color = group), size = 2) +
      geom_errorbar(data = bin_summary, aes(x = x, ymin = ci_low, ymax = ci_high, color = group), width = 0.1)
  }

  if (!is.null(model_fit) && y_scale == "predicted") {
    p1 <- p1 + geom_line(data = pred_data, aes(x = exposure, y = pred), color = "black")
  }

  p1 <- p1 + scale_color_manual(values = palette)
  if (logx) p1 <- p1 + scale_x_log10()

  # Replace Panel 2 with boxplots of exposure by dose and group
  p2 <- ggplot(df, aes(x = exposure, y = factor(dose), fill = group)) +
    geom_boxplot(outlier.alpha = 0.2, width = 0.5) +
    labs(x = "Exposure", y = "Dose") +
    scale_fill_manual(values = palette) +
    theme_bw()

  if (logx) p2 <- p2 + scale_x_log10()

  combined <- p1 / p2 + plot_layout(heights = c(3, 1))

  print(combined)
  if (return_data) return(list(summary = bin_summary, model = model_fit))
}

# Examples
#
# set.seed(123)
# n <- 600
# sim_data <- tibble(
#   subject = 1:n,
#   dose = rep(c(10, 30, 100), each = n / 3),
#   clearance = rlnorm(n, log(10), 0.4),
#   group = rep(c("A", "B"), length.out = n)
# ) %>%
#   mutate(
#     exposure = dose / clearance,
#     logit_p = -2 + 0.8 * log(exposure) + ifelse(group == "B", 0.5, 0),
#     response = rbinom(n, size = 1, prob = plogis(logit_p))
#   )
#
# plot_logistic_exposure_response(
#   data = sim_data,
#   dose_var = "dose",
#   exposure_var = "exposure",
#   response_var = "response",
#   group_var = "group",
#   y_scale = "proportion"
# )
#
# plot_logistic_exposure_response(
#   data = sim_data,
#   dose_var = "dose",
#   exposure_var = "exposure",
#   response_var = "response",
#   group_var = "group",
#   y_scale = "predicted"
# )
