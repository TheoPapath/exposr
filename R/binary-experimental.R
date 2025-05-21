# ## Binary extension (in progress)
# ## Detect binary endpoint and redirect to binary plotting logic
#
# plot_exposure_response <- function(data, dose_var, exposure_var, response_var,
#                                    table_summary = FALSE,
#                                    show_median_lines = TRUE,
#                                    show_points = FALSE,
#                                    effect_label_position = c("left", "right"),
#                                    show_effect_lines = "none",
#                                    external_df = NULL, external_mapping = NULL,
#                                    xlims = NULL, ylims = NULL,
#                                    vertical_line = NULL,
#                                    xlab = NULL, ylab_top = NULL, ylab_bottom = NULL,
#                                    log_scale = c("none", "logx", "logy", "logxy"),
#                                    n_bins = 6, summary_type = c("geometric", "median", "mean"),
#                                    add_trend = TRUE, trend_method = c("loess", "lm", "gam", "glm"),
#                                    trend_ci = TRUE, trend_ci_level = 0.95,
#                                    group_var = NULL, covariate_var = NULL, endpt_list = NULL,
#                                    binary_override = NULL) {
#
#   # Load required libraries
#   library(ggplot2)
#   library(dplyr)
#   library(tidyr)
#   library(patchwork)
#   library(Hmisc)
#   library(rlang)
#   library(broom)
#   library(tibble)
#   library(cowplot)
#   library(purrr)
#   library(ggpubr)
#
#   # Detect binary outcome unless overridden
#   is_binary_response <- if (!is.null(binary_override)) {
#     binary_override
#   } else if (!is.null(response_var)) {
#     vals <- unique(data[[response_var]])
#     all(vals %in% c(0, 1)) && length(vals) <= 3
#   } else if (!is.null(endpt_list)) {
#     TRUE
#   } else {
#     FALSE
#   }
#
#   if (is_binary_response) {
#     message("[plot_exposure_response] Detected binary response variable — using logistic plotting logic.")
#     return(plot_exposure_response_binary(
#       table_summary = table_summary,
#       data = data,
#       dose_var = as.character(substitute(dose_var)),
#       exposure_var = as.character(substitute(exposure_var)),
#       response_var = if (!is.null(response_var)) as.character(substitute(response_var)) else NULL,
#       group_var = if (!is.null(group_var)) as.character(substitute(group_var)) else NULL,
#       covariate_var = if (!is.null(covariate_var)) as.character(substitute(covariate_var)) else NULL,
#       endpt_list = endpt_list,
#       show_median_lines = show_median_lines,
#       show_points = show_points,
#       xlims = xlims,
#       ylims = ylims,
#       xlab = xlab,
#       ylab_top = ylab_top,
#       ylab_bottom = ylab_bottom,
#       log_scale = log_scale,
#       n_bins = n_bins,
#       vertical_line = vertical_line,
#       trend_ci = trend_ci,
#       trend_ci_level = trend_ci_level,
#       add_trend = add_trend,
#       return_plot = TRUE
#     ))
#   }
#
#   # Continuous endpoint logic would follow here...
# }
#
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
#   # Define EC50 extraction function
#   extract_ec50 <- function(df) {
#     fit <- tryCatch(glm(Response ~ Exposure, family = binomial, data = df), error = function(e) NULL)
#     if (is.null(fit)) return("EC50 could not be estimated")
#     coefs <- coef(summary(fit))
#     if (nrow(coefs) < 2) return("EC50 estimation failed")
#     b0 <- coefs[1, 1]
#     b1 <- coefs[2, 1]
#     se_b0 <- coefs[1, 2]
#     se_b1 <- coefs[2, 2]
#     cov_b0b1 <- vcov(fit)[1, 2]
#     ec50 <- -b0 / b1
#     se_ec50 <- sqrt((se_b0^2 / b1^2) + (b0^2 * se_b1^2 / b1^4) + (2 * b0 * cov_b0b1 / b1^3))
#     ci <- ec50 + c(-1.96, 1.96) * se_ec50
#     sprintf("Estimated EC50: %.2f (95%% CI %.2f–%.2f)", ec50, ci[1], ci[2])
#   }
#
#   log_scale <- match.arg(log_scale)
#
#   # Direct referencing
#   data <- data %>%
#     mutate(
#       Dose = .data[[dose_var]],
#       Exposure = .data[[exposure_var]]
#     )
#
#   if (!is.null(response_var)) {
#     data <- data %>% mutate(Response = .data[[response_var]])
#   }
#
#   if (!is.null(group_var)) {
#     data <- data %>% mutate(Group = as.factor(.data[[group_var]]))
#   } else {
#     data <- data %>% mutate(Group = "All")
#   }
#
#   if (!is.null(covariate_var)) {
#     data <- data %>% mutate(Covariate = as.factor(.data[[covariate_var]]))
#     return(
#       plot_grid(
#         plotlist = lapply(split(data, data$Covariate), function(df) {
#           plot_exposure_response_binary(
#             data = df,
#             dose_var = dose_var,
#             exposure_var = exposure_var,
#             response_var = response_var,
#             group_var = group_var,
#             covariate_var = NULL,
#             endpt_list = endpt_list,
#             show_median_lines = show_median_lines,
#             show_points = show_points,
#             xlims = xlims,
#             ylims = ylims,
#             xlab = xlab,
#             ylab_top = ylab_top,
#             ylab_bottom = ylab_bottom,
#             log_scale = log_scale,
#             n_bins = n_bins,
#             vertical_line = vertical_line,
#             trend_ci = trend_ci,
#             trend_ci_level = trend_ci_level,
#             add_trend = add_trend,
#             return_plot = TRUE
#           )
#         }),
#         ncol = 1
#       )
#     )
#   }
#
#   # MULTI-ENDPOINT MODE
#   if (!is.null(endpt_list)) {
#     long_df <- data %>%
#       select(Exposure, Dose, all_of(endpt_list)) %>%
#       pivot_longer(cols = all_of(endpt_list), names_to = "Endpoint", values_to = "Response") %>%
#       filter(Dose > 0) %>%
#       mutate(Exposure_bin = cut2(Exposure, g = n_bins))
#
#     summary_df <- long_df %>%
#       group_by(Endpoint, Exposure_bin) %>%
#       summarise(x = median(Exposure),
#                 N = n(),
#                 n = sum(Response),
#                 rate = mean(Response),
#                 ylo = binom.test(n, N)$conf.int[1],
#                 yhi = binom.test(n, N)$conf.int[2], .groups = "drop")
#
#     p1 <- ggplot(long_df, aes(x = Exposure, y = Response, color = Endpoint)) +
#       geom_hline(yintercept = c(0, 1), color = "grey70") +
#       geom_smooth(method = "glm", method.args = list(family = binomial), se = trend_ci, level = trend_ci_level) +
#       geom_point(data = summary_df, mapping = aes(x = x, y = rate), inherit.aes = FALSE) +
#       geom_errorbar(data = summary_df, mapping = aes(x = x, ymin = ylo, ymax = yhi, color = Group), width = 0.05, inherit.aes = FALSE) +
#       theme_minimal() +
#       labs(x = xlab %||% "Exposure", y = ylab_top %||% "Probability", color = "Endpoint")
#
#     if (log_scale %in% c("logx", "logxy")) p1 <- p1 + scale_x_log10()
#
#     med_exp <- long_df %>% group_by(Dose) %>% summarise(med_exp = median(Exposure), .groups = "drop")
#     p2 <- long_df %>%
#       ggplot(aes(x = Exposure, y = factor(Dose))) +
#       geom_boxplot(fill = "white", outlier.shape = NA) +
#       { if (show_median_lines) geom_vline(data = med_exp, aes(xintercept = med_exp), linetype = "dotted", color = "grey30") } +
#       labs(x = xlab %||% "Exposure", y = ylab_bottom %||% "Dose") +
#       theme_minimal()
#
#     if (log_scale %in% c("logx", "logxy")) p2 <- p2 + scale_x_log10()
#
#     if (table_summary) {
#       table_data <- summary_df %>%
#         select(Group, Exposure_bin, n, N, rate_label) %>%
#         mutate(rate = paste0(n, "/", N, " (", rate_label, ")")) %>%
#         select(Group, Exposure_bin, rate) %>%
#         pivot_wider(names_from = Group, values_from = rate)
#
#       table_plot <- ggtexttable(table_data, rows = NULL, theme = ttheme(base_style = "classic", base_size = 9))
#       final <- plot_grid((p1 / p2) + plot_layout(heights = c(4, 1)), table_plot, ncol = 1, rel_heights = c(1, 0.3))
#     } else {
#       if (table_summary) {
#         table_data <- summary_df %>%
#           select(Group, Exposure_bin, n, N, rate_label) %>%
#           mutate(rate = paste0(n, "/", N, " (", rate_label, ")")) %>%
#           select(Group, Exposure_bin, rate) %>%
#           pivot_wider(names_from = Group, values_from = rate)
#
#         table_plot <- ggtexttable(table_data, rows = NULL, theme = ttheme(base_style = "classic", base_size = 9))
#         final <- plot_grid((p1 / p2) + plot_layout(heights = c(4, 1)), table_plot, ncol = 1, rel_heights = c(1, 0.3))
#       } else {
#         if (table_summary) {
#           table_data <- summary_df %>%
#             select(Group, Exposure_bin, n, N, rate_label) %>%
#             mutate(rate = paste0(n, "/", N, " (", rate_label, ")")) %>%
#             select(Group, Exposure_bin, rate) %>%
#             pivot_wider(names_from = Group, values_from = rate)
#
#           table_plot <- ggtexttable(table_data, rows = NULL, theme = ttheme(base_style = "classic", base_size = 9))
#           final <- plot_grid((p1 / p2) + plot_layout(heights = c(4, 1)), table_plot, ncol = 1, rel_heights = c(1, 0.3))
#         } else {
#           if (table_summary) {
#             table_data <- summary_df %>%
#               mutate(rate_label = paste0(round(100 * rate, 1), "%")) %>%
#               select(Group, Exposure_bin, n, N, rate_label) %>%
#               mutate(rate = paste0(n, "/", N, " (", rate_label, ")")) %>%
#               select(Group, Exposure_bin, rate) %>%
#               pivot_wider(names_from = Group, values_from = rate)
#
#             table_plot <- ggtexttable(table_data, rows = NULL, theme = ttheme(base_style = "classic", base_size = 9))
#             final <- plot_grid((p1 / p2) + plot_layout(heights = c(4, 1)), table_plot, ncol = 1, rel_heights = c(1, 0.3))
#           } else {
#             final <- (p1 / p2) + plot_layout(heights = c(4, 1))
#           }
#         }
#       }
#     }
#     if (return_plot) return(final) else print(final)
#   }
#
#   if (log_scale %in% c("logx", "logxy")) {
#     data <- data %>% filter(Exposure > 0)
#   }
#
#   data <- data %>%
#     filter(Dose > 0) %>%
#     group_by(Group) %>%
#     mutate(Exposure_bin = cut2(Exposure, g = n_bins)) %>%
#     ungroup()
#
#   summary_df <- data %>%
#     group_by(Group, Exposure_bin) %>%
#     summarise(
#       x = median(Exposure),
#       x = median(Exposure),
#       midptx = mean(range(Exposure)),
#       N = n(),
#       n = sum(Response),
#       rate = mean(Response),
#       rate_label = paste0(round(rate * 100, 1), "%"),
#       ylo = binom.test(n, N)$conf.int[1],
#       yhi = binom.test(n, N)$conf.int[2],
#       .groups = "drop"
#     )
#
#   p1 <- ggplot(data, aes(x = Exposure, y = Response, color = Group)) +
#     scale_color_manual(values = if (is.null(group_var)) c("All" = "black") else RColorBrewer::brewer.pal(8, "Set1")) +
#     { if (is.null(group_var)) theme(legend.position = "none") } +
#     geom_hline(yintercept = c(0, 1), color = "grey70") +
#     { if (add_trend) geom_smooth(method = "glm", method.args = list(family = binomial), se = trend_ci, level = trend_ci_level) } +
#     geom_point(data = summary_df, mapping = aes(x = x, y = rate, color = Group), inherit.aes = FALSE, size = 2) +
#     geom_errorbar(data = summary_df, mapping = aes(x = x, ymin = ylo, ymax = yhi, color = Group), width = 0.05, inherit.aes = FALSE) +
#     labs(x = xlab %||% "Exposure", y = ylab_top %||% "Response Probability", caption = extract_ec50(data)) +
#     coord_cartesian(xlim = xlims, ylim = c(0, 1.05)) +
#     theme_minimal() +
#     { if (is.null(group_var)) theme(legend.position = "none") } +
#     {
#       # Add p-value annotations
#       pvals <- data %>% group_by(Group) %>% summarise(
#         pval = summary(glm(Response ~ Exposure, family = binomial, data = cur_data()))$coefficients[2, 4],
#         .groups = "drop"
#       ) %>%
#         mutate(label = paste0("p = ", ifelse(pval < 0.0001, "<0.0001", signif(pval, 3))),
#                xpos = max(data$Exposure, na.rm = TRUE) * 0.98,
#                ypos = seq(0.1, 0.1 + 0.1 * (n() - 1), length.out = n()))
#
#       geom_text(data = pvals, aes(x = xpos, y = ypos, label = label, color = Group), inherit.aes = FALSE, hjust = 1, size = 3.5)
#     } +
#     { if (is.null(group_var)) theme(legend.position = "none") }
#
#
#   if (show_points) {
#     p1 <- p1 + geom_jitter(width = 0, height = 0.02, alpha = 0.3)
#   }
#   if (!is.null(vertical_line)) {
#     p1 <- p1 + geom_vline(xintercept = vertical_line, linetype = "dashed")
#   }
#   if (log_scale %in% c("logx", "logxy")) {
#     p1 <- p1 + scale_x_log10()
#   }
#
#   med_exp <- data %>% group_by(Dose) %>% summarise(med_exp = median(Exposure), .groups = "drop")
#   p2 <- data %>%
#     ggplot(aes(x = Exposure, y = factor(Dose))) +
#     geom_boxplot(fill = "white", outlier.shape = NA) +
#     { if (show_median_lines) geom_vline(data = med_exp, aes(xintercept = med_exp), linetype = "dotted", color = "grey30") } +
#     labs(x = xlab %||% "Exposure", y = ylab_bottom %||% "Dose") +
#     theme_minimal()
#
#   if (log_scale %in% c("logx", "logxy")) {
#     p2 <- p2 + scale_x_log10()
#   }
#
#   final <- (p1 / p2) + plot_layout(heights = c(4, 1))
#   if (return_plot) return(final) else print(final)
# }
#
#
# # --- Example Usage ---
#
# # Simulate mock dataset
# set.seed(42)
# n <- 600
# df_binary <- tibble(
#   ID = 1:n,
#   Dose = rep(c(0, 50, 100, 300, 600), each = n / 5),
#   Sex = sample(c("M", "F"), n, replace = TRUE),
#   Treatment = sample(c("A", "B"), n, replace = TRUE)
# )
# df_binary <- df_binary %>%
#   mutate(
#     Exposure = Dose * runif(n, 0.8, 1.2),
#     Response1 = rbinom(n, 1, prob = plogis((Exposure - 100) / 100)),
#     Response2 = rbinom(n, 1, prob = plogis((Exposure - 200) / 120)),
#     Response3 = rbinom(n, 1, prob = plogis((Exposure - 300) / 150)),
#     Response = Response1  # default endpoint
#   )
#
# # 1. Single binary endpoint, no grouping
# plot_exposure_response(
#   df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = "Response",
#   show_points = TRUE
# )
#
# # 2. Grouped by treatment (color-coded)
# plot_exposure_response(
#   df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = "Response",
#   group_var = "Treatment", show_points = TRUE
# )
#
# # 3. Stratified by covariate (sex)
# plot_exposure_response(
#   df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = "Response",
#   group_var = "Treatment", covariate_var = "Sex", show_points = TRUE
# )
#
# # 4. Multiple binary endpoints on one plot
# plot_exposure_response(
#   df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = NULL,
#   endpt_list = c("Response1", "Response2", "Response3")
# )
#
# # 5. Tabular summary enabled
# plot_exposure_response(
#   df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = "Response",
#   group_var = "Treatment", table_summary = TRUE
# )
#
# # 6. Tabular summary with covariate stratification
# plot_exposure_response(
#   df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = "Response",
#   group_var = "Treatment", covariate_var = "Sex", table_summary = TRUE
# )
#
# # 4. Multiple binary endpoints on one plot
# plot_exposure_response(
#   df_binary, dose_var = "Dose", exposure_var = "Exposure", response_var = NULL,
#   endpt_list = c("Response1", "Response2", "Response3")
# )
