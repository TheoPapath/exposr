#' Plot Dose-Response Relationship
#'
#' Visualizes the dose-response relationship with optional model fits and confidence intervals.
#' Can be used for continuous endpoints or binary outcomes (0/1).
#'
#' @param data A data frame containing the dose and response values.
#' @param dose_var Name of the column representing the dose variable.
#' @param response_var Name of the column representing the response variable.
#' @param group_var Optional; name of grouping variable for stratified plotting.
#' @param connect Logical; whether to connect mean points by line (default = TRUE).
#' @param fit_model Logical; whether to overlay a logistic/Emax model fit (default = TRUE).
#' @param show_ci Logical; whether to include a confidence interval around the model fit (default = TRUE).
#' @param return_fit Logical; if TRUE, return the fitted model as an attribute (default = FALSE).
#' @param color_palette Optional vector of colors to apply per group.
#' @param x_lab Optional x-axis label.
#' @param y_lab Optional y-axis label.
#' @param log_scale Logical; whether to log-transform x-axis (default = FALSE).
#' @param title Optional title.
#'
#' @return A ggplot object (with optional model fit attached as attribute).
#' @export
#'
#' @examples
#' # Continuous response
#' set.seed(123)
#' df <- data.frame(
#'   dose = rep(c(0, 5, 10, 20, 40, 80), each = 20),
#'   response = c(
#'     rnorm(20, 10, 3),
#'     rnorm(20, 20, 3),
#'     rnorm(20, 30, 3),
#'     rnorm(20, 45, 3),
#'     rnorm(20, 55, 3),
#'     rnorm(20, 60, 3)
#'   )
#' )
#' plot_dose_response(df, dose_var = "dose", response_var = "response")
#'
#' # Binary response with CI and binomial error bars
#' set.seed(321)
#' df_bin <- data.frame(
#'   dose = rep(c(0, 5, 10, 20, 40, 80), each = 50)
#' )
#' df_bin$response <- rbinom(nrow(df_bin), 1, plogis((df_bin$dose - 20) / 10))
#' plot_dose_response(df_bin, dose_var = "dose", response_var = "response")

plot_dose_response <- function(
    data,
    dose_var,
    response_var,
    group_var = NULL,
    connect = TRUE,
    fit_model = TRUE,
    show_ci = TRUE,
    return_fit = FALSE,
    color_palette = NULL,
    x_lab = NULL,
    y_lab = NULL,
    log_scale = FALSE,
    title = "Dose-Response Curve"
) {
  library(dplyr)
  library(ggplot2)

  data <- data %>% mutate(dose = .data[[dose_var]], response = .data[[response_var]])
  if (!is.null(group_var)) {
    data <- data %>% mutate(group = as.factor(.data[[group_var]]))
  } else {
    data$group <- factor("Overall")
  }

  is_binary <- all(data$response %in% c(0, 1))

  summary_df <- data %>%
    group_by(group, dose) %>%
    summarise(
      mean_response = mean(response, na.rm = TRUE),
      se = if (is_binary) sqrt(mean_response * (1 - mean_response) / n()) else sd(response, na.rm = TRUE) / sqrt(n()),
      n = n(),
      .groups = "drop"
    ) %>%
    mutate(
      lower = pmax(0, mean_response - 1.96 * se),
      upper = pmin(1, mean_response + 1.96 * se)
    )

  p <- ggplot() +
    geom_jitter(data = data, aes(x = dose, y = response, color = group),
                width = 0.05 * diff(range(data$dose, na.rm = TRUE)),
                height = 0.05 * (1 - is_binary), alpha = 0.3) +
    geom_point(data = summary_df, aes(x = dose, y = mean_response, color = group), size = 2.5)

  if (show_ci && is_binary) {
    p <- p + geom_errorbar(data = summary_df, aes(x = dose, ymin = lower, ymax = upper, color = group), width = 0.1)
  }

  if (connect) {
    p <- p + geom_line(data = summary_df, aes(x = dose, y = mean_response, color = group))
  }

  fit_list <- list()
  if (fit_model) {
    for (g in levels(data$group)) {
      data_sub <- data %>% filter(group == g)
      fit <- try({
        if (is_binary) {
          glm(response ~ dose, data = data_sub, family = binomial)
        } else {
          nls(response ~ E0 + Emax * dose / (ED50 + dose),
              data = data_sub,
              start = list(E0 = min(data_sub$response), Emax = max(data_sub$response), ED50 = median(data_sub$dose)))
        }
      }, silent = TRUE)

      if (!inherits(fit, "try-error")) {
        pred_df <- data.frame(dose = seq(min(data$dose), max(data$dose), length.out = 200))
        if (is_binary) {
          preds <- predict(fit, newdata = pred_df, type = "link", se.fit = TRUE)
          pred_df$response <- plogis(preds$fit)
          pred_df$lower <- plogis(preds$fit - 1.96 * preds$se.fit)
          pred_df$upper <- plogis(preds$fit + 1.96 * preds$se.fit)
          pred_df$group <- g
          if (show_ci) {
            p <- p + geom_ribbon(data = pred_df, aes(x = dose, ymin = lower, ymax = upper, fill = group),
                                 alpha = 0.2, inherit.aes = FALSE)
          }
        } else {
          pred_df$response <- predict(fit, newdata = pred_df)
          pred_df$group <- g
        }
        p <- p + geom_line(data = pred_df, aes(x = dose, y = response, color = group), linetype = "dashed", linewidth = 1)
        fit_list[[g]] <- fit
      }
    }
  }

  if (!is.null(color_palette)) {
    p <- p + scale_color_manual(values = color_palette)
  }
  if (!is.null(x_lab)) p <- p + xlab(x_lab)
  if (!is.null(y_lab)) p <- p + ylab(y_lab)
  if (log_scale) p <- p + scale_x_log10()

  p <- p +
    theme_minimal() +
    labs(title = title, color = if (!is.null(group_var)) group_var else NULL)

  if (return_fit) {
    attr(p, "fit") <- fit_list
  }

  p <- p +
    labs(caption = if (is_binary) {
      "Proportion of responders shown with binomial 95% confidence intervals and logistic model fit (dashed). Shaded ribbons indicate model-predicted 95% CI."
    } else {
      "Mean response with standard error bars and Emax model fit (dashed)."
    })

  return(p)
}
