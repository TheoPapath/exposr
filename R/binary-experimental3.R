# # --- Binary Logistic Plot Implementation ---
# plot_exposure_response_binary <- function(data, dose_var, exposure_var, response_var,
#                                           table_summary = FALSE,
#                                           group_var = NULL, covariate_var = NULL, endpt_list = NULL,
#                                           show_median_lines = TRUE,
#                                           show_points = FALSE,
#                                           xlims = NULL, ylims = NULL,
#                                           xlab = NULL, ylab_top = NULL, ylab_bottom = NULL,
#                                           log_scale = c("none", "logx", "logy", "logxy"),
#                                           n_bins = 6, vertical_line = NULL,
#                                           trend_ci = TRUE, trend_ci_level = 0.95,
#                                           add_trend = TRUE, return_plot = TRUE) {
#
#   library(ggplot2)
#   library(dplyr)
#   library(tidyr)
#   library(purrr)
#   library(patchwork)
#   library(ggpubr)
#   library(Hmisc)
#
#   extract_ec50 <- function(df) {
#     fit <- tryCatch(glm(Response ~ Exposure, family = binomial, data = df), error = function(e) NULL)
#     if (is.null(fit)) return("EC50 could not be estimated")
#     b <- coef(summary(fit))
#     if (nrow(b) < 2) return("EC50 estimation failed")
#     ec50 <- -b[1, 1] / b[2, 1]
#     se_ec50 <- sqrt((b[1, 2]^2 / b[2, 1]^2) + (b[1, 1]^2 * b[2, 2]^2 / b[2, 1]^4) +
#                       2 * b[1, 1] * vcov(fit)[1, 2] / b[2, 1]^3)
#     ci <- ec50 + c(-1.96, 1.96) * se_ec50
#     sprintf("Estimated EC50: %.2f (95%% CI %.2f–%.2f)", ec50, ci[1], ci[2])
#   }
#
#   plot_core <- function(data, group_lab, color_lab) {
#     data <- data %>% filter(Dose > 0)
#     if (log_scale %in% c("logx", "logxy")) data <- data %>% filter(Exposure > 0)
#     data <- data %>% mutate(Exposure_bin = cut2(Exposure, g = n_bins))
#
#     summary_df <- data %>%
#       group_by(.data[[group_lab]], Exposure_bin) %>%
#       summarise(
#         x = median(Exposure),
#         midptx = mean(range(Exposure)),
#         N = n(),
#         n = sum(Response),
#         rate = mean(Response),
#         rate_label = paste0(round(rate * 100, 1), "%"),
#         ylo = binom.test(n, N)$conf.int[1],
#         yhi = binom.test(n, N)$conf.int[2],
#         .groups = "drop"
#       )
#
#     color_levels <- levels(as.factor(data[[group_lab]]))
#     colors <- setNames(RColorBrewer::brewer.pal(max(length(color_levels), 3), "Set1")[seq_along(color_levels)], color_levels)
#
#     p1 <- ggplot(data, aes(x = Exposure, y = Response, color = .data[[group_lab]])) +
#       geom_hline(yintercept = c(0, 1), color = "grey70") +
#       { if (add_trend) geom_smooth(method = "glm", method.args = list(family = binomial), se = trend_ci, level = trend_ci_level) } +
#       geom_point(data = summary_df, aes(x = x, y = rate, color = .data[[group_lab]]), inherit.aes = FALSE, size = 2) +
#       geom_errorbar(data = summary_df, aes(x = x, ymin = ylo, ymax = yhi, color = .data[[group_lab]]), width = 0.05, inherit.aes = FALSE) +
#       scale_color_manual(values = colors) +
#       labs(x = xlab %||% "Exposure", y = ylab_top %||% "Response Probability", caption = extract_ec50(data), color = color_lab) +
#       coord_cartesian(xlim = xlims, ylim = c(0, 1.05)) +
#       theme_minimal()
#
#     if (log_scale %in% c("logx", "logxy")) p1 <- p1 + scale_x_log10()
#     if (show_points) p1 <- p1 + geom_jitter(width = 0, height = 0.02, alpha = 0.3)
#     if (!is.null(vertical_line)) p1 <- p1 + geom_vline(xintercept = vertical_line, linetype = "dashed")
#
#     p2 <- ggplot(data, aes(x = Exposure, y = factor(Dose))) +
#       geom_boxplot(fill = "white", outlier.shape = NA) +
#       { if (show_median_lines) geom_vline(data = data %>% group_by(Dose) %>% summarise(med_exp = median(Exposure), .groups = "drop"), aes(xintercept = med_exp), linetype = "dotted", color = "grey30") } +
#       labs(x = xlab %||% "Exposure", y = ylab_bottom %||% "Dose") +
#       theme_minimal()
#
#     if (log_scale %in% c("logx", "logxy")) p2 <- p2 + scale_x_log10()
#
#     (p1 / p2) + plot_layout(heights = c(4, 1))
#   }
#
#   log_scale <- match.arg(log_scale)
#   data <- data %>%
#     mutate(
#       Dose = .data[[dose_var]],
#       Exposure = .data[[exposure_var]],
#       Response = if (!is.null(response_var)) .data[[response_var]] else NA,
#       Group = if (!is.null(group_var)) as.factor(.data[[group_var]]) else factor("All")
#     )
#
#   if (!is.null(covariate_var)) {
#     data <- data %>% mutate(Covariate = as.factor(.data[[covariate_var]]))
#     plotlist <- split(data, data$Covariate) %>%
#       map(~ plot_core(.x, group_lab = "Group", color_lab = "Group"))
#     return(plot_grid(plotlist = plotlist, ncol = 1))
#   }
#
#   if (!is.null(endpt_list)) {
#     data_long <- data %>%
#       select(Exposure, Dose, all_of(endpt_list)) %>%
#       pivot_longer(cols = all_of(endpt_list), names_to = "Endpoint", values_to = "Response") %>%
#       mutate(Group = as.factor(Endpoint))
#     return(plot_core(data_long, group_lab = "Group", color_lab = "Endpoint"))
#   }
#
#   final <- plot_core(data, group_lab = "Group", color_lab = "Group")
#   if (return_plot) return(final) else print(final)
# }
#
# # --- Example Usage ---
#
# set.seed(42)
# n <- 600
# df_binary <- tibble(
#   ID = 1:n,
#   Dose = rep(c(0, 50, 100, 300, 600), each = n / 5),
#   Sex = sample(c("M", "F"), n, replace = TRUE),
#   Treatment = sample(c("A", "B"), n, replace = TRUE)
# ) %>%
#   mutate(
#     Exposure = Dose * runif(n, 0.8, 1.2),
#     Response1 = rbinom(n, 1, prob = plogis((Exposure - 100) / 100)),
#     Response2 = rbinom(n, 1, prob = plogis((Exposure - 200) / 120)),
#     Response3 = rbinom(n, 1, prob = plogis((Exposure - 300) / 150)),
#     Response = Response1
#   )
#
# plot_exposure_response_binary(df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = "Response", show_points = TRUE)
# plot_exposure_response_binary(df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = "Response", group_var = "Treatment", show_points = TRUE)
# plot_exposure_response_binary(df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = "Response", group_var = "Treatment", covariate_var = "Sex", show_points = TRUE)
# plot_exposure_response_binary(df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = NULL, endpt_list = c("Response1", "Response2", "Response3"))
