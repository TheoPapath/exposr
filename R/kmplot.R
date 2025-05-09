#' Kaplan-Meier Plot for Continuous or Categorical Covariates
#'
#' This function generates a Kaplan-Meier plot for time-to-event data, stratified
#' either by quartiles of a continuous covariate or by levels of a categorical covariate.
#'
#' @param data A data.frame containing the input data.
#' @param time_var Character. Name of the time-to-event variable.
#' @param event_var Character. Name of the event indicator variable (0 = censored, 1 = event).
#' @param covariate Character. Name of the covariate to stratify by.
#' @param covariate_type Character. One of "continuous" or "categorical".
#' @param covariate_labels Optional named vector to rename levels of the covariate.
#' @param covariate_label Optional character. Label for the covariate (used in legend and risk table).
#' @param time_label Optional character. Label for the x-axis.
#' @param survival_label Optional character. Label for the y-axis.
#' @param colors Optional character vector of colors for the groups.
#' @param time_break Numeric. Interval for x-axis breaks (default is 1).
#' @param plot_ci Logical. Whether to show confidence intervals (default = FALSE).
#' @param show_pval Logical. Whether to display the p-value (default = FALSE).
#' @param show_fit Logical. Whether to print the survfit object (default = TRUE).
#' @param cumulative Logical. Whether to plot cumulative incidence instead of survival (default = FALSE).
#' @param return_plot Logical. Whether to return the ggplot object (default = TRUE).
#'
#' @return A Kaplan-Meier plot (ggplot2 object) with optional risk table.
#' @export
#'
#' @examples
#' library(survival)
#' library(survminer)
#' data(lung)
#'
#' # Example 1: Categorical covariate (sex)
#' kmplot(data = lung,
#' time_var = "time",
#'     event_var = "status",
#'     covariate = "sex",
#'     covariate_type = "categorical",
#'     covariate_labels = c("1" = "Male", "2" = "Female"),
#'     covariate_label = "Sex",
#'     time_label = "Time (days)",
#'     survival_label = "Survival Probability",
#'     time_break = 100,
#'     show_pval = TRUE)
#
#' # Example 2: Continuous covariate (age)
#' kmplot(data = lung,
#'     time_var = "time",
#'     event_var = "status",
#'     covariate = "age",
#'     covariate_type = "continuous",
#'     covariate_label = "Age Quartiles",
#'     time_label = "Time (days)",
#'     survival_label = "Survival Probability",
#'     time_break = 100,
#'     show_pval = TRUE)

kmplot <- function(data, time_var, event_var, covariate, covariate_type = c("continuous", "categorical"),
                   covariate_labels = NULL, covariate_label = NULL,
                   time_label = NULL, survival_label = NULL,
                   colors = NULL, time_break = 1,
                   plot_ci = FALSE, show_pval = FALSE,
                   show_fit = TRUE, cumulative = FALSE,
                   return_plot = TRUE,
                   show_summary_box = TRUE,
                   summary_box_position = c("topright", "topleft", "bottomright", "bottomleft")) {
  library(survival)
  library(survminer)
  library(dplyr)
  library(ggplot2)
  library(scales)

  covariate_type <- match.arg(covariate_type)

  data <- data %>%
    mutate(time = .data[[time_var]],
           event = .data[[event_var]],
           cov = .data[[covariate]])

  if (is.null(time_label)) time_label <- time_var
  if (is.null(survival_label)) survival_label <- ifelse(cumulative, "Cumulative Incidence", "Survival Probability")
  if (is.null(covariate_label)) covariate_label <- covariate

  if (covariate_type == "continuous") {
    data <- data %>%
      mutate(cov_group = cut(cov, quantile(cov, probs = seq(0, 1, 0.25), na.rm = TRUE), include.lowest = TRUE)) %>%
      group_by(cov_group) %>%
      mutate(medcov = signif(median(cov, na.rm = TRUE), 3)) %>%
      ungroup() %>%
      mutate(lbl = paste0("Q", as.integer(as.factor(cov_group)), ": ", medcov))
  } else {
    data <- data %>%
      mutate(lbl = as.character(cov))
    if (!is.null(covariate_labels)) {
      data <- data %>% mutate(lbl = as.character(covariate_labels[as.character(cov)]))
    }
  }

  fit <- survfit(Surv(time, event) ~ lbl, data = data)
  if (show_fit) print(fit)

  pval_coord <- if (!show_pval) NULL else {
    x_pos <- ifelse(cumulative, max(data$time) * 0.05, max(data$time) * 0.8)
    c(x_pos, 1)
  }

  custom_theme <- theme_minimal(base_size = 13) +
    theme(
      legend.position = "top",
      legend.title = element_text(size = 11, face = "bold"),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank()
    )

  if (is.null(colors)) {
    n_groups <- length(unique(data[[covariate]]))
    colors <- exposr_palette(min(n_groups, 8))
  }

  plot <- suppressWarnings(
    ggsurvplot(fit, data = data,
               fun = if (cumulative) {function(x) 1 - x } else {NULL},
               conf.int = plot_ci,
               pval.coord = pval_coord,
               risk.table = TRUE,
               risk.table.y.text.col = TRUE,
               risk.table.col = "lbl",
               legend.title = covariate_label,
               legend.labs = sort(unique(data$lbl)),
               palette = colors,
               break.time.by = time_break,
               surv.median.line = "hv",
               ggtheme = custom_theme,
               xlab = time_label,
               ylab = survival_label) )

  median_annot <- summary(fit)$table
  if (is.null(dim(median_annot))) {
    label_df <- data.frame(
      x = median_annot["median"],
      y = 0.05,
      label = paste0("Median: ", signif(median_annot["median"], 3))
    )
  } else {
    label_df <- data.frame(
      x = median_annot[, "median"],
      y = 0.05,
      strata = rownames(median_annot),
      label = paste0(rownames(median_annot), ": ", signif(median_annot[, "median"], 3))
    )
  }

  summary_box_position <- match.arg(summary_box_position)

  summary_text <- if (is.null(dim(median_annot))) {
    events <- sum(data$event == 1)
    paste0("Events: ", events, " | Median: ", signif(median_annot["median"], 3))
  } else {
    event_counts <- median_annot[, "events"]
    median_vals <- paste0(
      signif(median_annot[, "median"], 3),
      " (",
      signif(median_annot[, "0.95LCL"], 3),
      "–",
      signif(median_annot[, "0.95UCL"], 3), ")"
    )
    groups <- gsub("^lbl=", "", rownames(median_annot))
    paste0(groups, ": ", event_counts, " events | Median: ", median_vals) %>% paste(collapse = "
")
  }

  if (show_pval) {
    surv_diff <- survdiff(Surv(time, event) ~ lbl, data = data)
    pval <- 1 - pchisq(surv_diff$chisq, length(surv_diff$n) - 1)
    summary_text <- paste0(summary_text, "
Log-rank p = ", format.pval(pval, digits = 3, eps = 1e-3))
  }

  if (covariate_type == "categorical" && length(unique(data$lbl)) == 2) {
    cox_fit <- coxph(Surv(time, event) ~ lbl, data = data)
    hr <- exp(coef(cox_fit))
    ci <- exp(confint(cox_fit))
    hr_ref <- levels(factor(data$lbl))[1]
    hr_comp <- levels(factor(data$lbl))[2]
    hr_text <- paste0("HR (", hr_comp, " vs ", hr_ref, ") = ",
                      signif(hr, 3), " (", signif(ci[1], 3), "–", signif(ci[2], 3), ")")
    summary_text <- paste0(summary_text, "
", hr_text)
  }

  if (show_summary_box) {
    pos_map <- list(
      topright = c(hjust = 1.05, vjust = 1.1, x = Inf, y = Inf),
      topleft = c(hjust = -0.05, vjust = 1.1, x = -Inf, y = Inf),
      bottomright = c(hjust = 1.05, vjust = -0.1, x = Inf, y = -Inf),
      bottomleft = c(hjust = -0.05, vjust = -0.1, x = -Inf, y = -Inf)
    )
    pos <- pos_map[[summary_box_position]]
    plot$plot <- plot$plot +
      annotate("label", x = pos[["x"]], y = pos[["y"]],
               hjust = pos[["hjust"]], vjust = 1,
               label = summary_text, size = 3.3, fontface = "plain",
               label.size = 0.3, fill = "white", lineheight = 1.1)
  }


  plot$plot <- plot$plot +
    labs(caption = {
      base <- if (cumulative) {
        "Lines represent cumulative incidence over time. Each step reflects a new event."
      } else {
        "Lines represent Kaplan–Meier survival estimates. Each step reflects a new event."
      }
      if (plot_ci) paste0(base, "
Shaded ribbons indicate 95% confidence intervals.") else base
    })

  print(plot)

  if (return_plot) {
    full_plot <- survminer:::.build_ggsurvplot(x = plot,
                                               surv.plot.height = NULL,
                                               risk.table.height = NULL,
                                               ncensor.plot.height = NULL)
    attr(full_plot, "meta") <- list(
      meta.type = "TTE_plot",
      meta.plot.vars = paste(time_var, event_var, covariate, sep = " / ")
    )
    return(full_plot)
  }
}
