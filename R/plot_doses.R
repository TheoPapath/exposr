#' Plot Dose-Level Configuration for Phase II Scenarios
#'
#' @import ggplot2
#' @import tibble
#' @import dplyr
#' @import tidyr
#' @import forcats
#'
#' @param dose_scenarios A named list of numeric vectors, each representing a dose scenario.
#' @param dose_labels A named character vector with human-readable descriptions for each scenario.
#' @param ed50_value Numeric value indicating ED50 dose.
#' @param ed90_value Numeric value indicating ED90 dose.
#' @param recommended_doses Optional numeric vector of recommended or anchor doses to highlight.
#' @param dose_strengths Optional numeric vector for vertical reference lines (e.g. available tablets).
#'
#' @param show_y_axis Logical; if FALSE and dose_labels are provided, hides y-axis labels.
#'
#' @return A ggplot object
#'
#' @examples
#' dose_scenarios <- list(
#'   doses_1 = c(0, 5, 60, 160),
#'   doses_2 = c(0, 10, 60, 160),
#'   doses_3 = c(0, 40, 80, 160),
#'   doses_4 = c(0, 20, 60, 160),
#'   doses_5 = c(0, 10, 20, 60, 160)
#' )
#'
#' dose_labels <- c(
#'   doses_1 = "One dose <ED50; wide spacing",
#'   doses_2 = "One dose ≈ED50; 3x & 6x multiples",
#'   doses_3 = "Broad intermediate range",
#'   doses_4 = "3x intervals from low tablet",
#'   doses_5 = "Added low dose near ED50"
#' )
#'
#' plot_doses(
#'   dose_scenarios = dose_scenarios,
#'   dose_labels = dose_labels,
#'   ed50_value = 10,
#'   ed90_value = 50,
#'   recommended_doses = c(10, 160),
#'   dose_strengths = c(5, 10, 20, 40, 80),
#'   show_y_axis = FALSE
#' )
plot_doses <- function(dose_scenarios, dose_labels, ed50_value = NULL, ed90_value = NULL,
                       recommended_doses = NULL, tablet_strengths = NULL, show_y_axis = TRUE) {

  # Convert to tidy format and classify doses
  dose_df <- tibble::enframe(dose_scenarios, name = "Scenario", value = "Dose") %>%
    unnest(Dose) %>%
    mutate(
      Description = dose_labels[Scenario],
      DoseLabel = as.character(Dose),
      Intensity = factor(case_when(
        Dose == 0 ~ "Placebo",
        Dose <= quantile(Dose, 0.25) ~ "Low",
        Dose <= quantile(Dose, 0.75) ~ "Mid",
        TRUE ~ "High"
      ), levels = c("Placebo", "Low", "Mid", "High"))
    ) %>%
    add_count(Scenario, name = "DoseCount") %>%
    arrange(DoseCount, Scenario) %>%
    mutate(
      Scenario = factor(Scenario, levels = unique(Scenario)),
      Description = factor(Description, levels = unique(Description))
    )

  # Define fill colors for intensity
  intensity_colors <- c(
    Placebo = "gray70",
    Low = "lightblue",
    Mid = "dodgerblue",
    High = "darkblue"
  )

  # Create base plot
  p <- ggplot(dose_df, aes(x = Dose, y = Scenario)) +
    geom_point(aes(fill = Intensity), shape = 21, size = 8, color = "black", stroke = 1.2) +
    geom_text(aes(label = DoseLabel), color = "white", size = 3)

  # Optional: add ED50 reference line
  if (!is.null(ed50_value)) {
    p <- p +
      geom_vline(xintercept = ed50_value, linetype = "dotted", color = "black", linewidth = 1) +
      annotate("text", x = ed50_value + 2, y = -Inf, label = "ED50", color = "black", hjust = 0, vjust = -1, size = 3)
  }

  # Optional: add ED90 reference line
  if (!is.null(ed90_value)) {
    p <- p +
      geom_vline(xintercept = ed90_value, linetype = "dotted", color = "black", linewidth = 1) +
      annotate("text", x = ed90_value + 2, y = -Inf, label = "ED90", color = "black", hjust = 0, vjust = -1, size = 3)
  }


  # Optional: add tablet strength reference lines
  if (!is.null(tablet_strengths)) {
    p <- p +
      geom_vline(xintercept = tablet_strengths, linetype = "dotted", color = "gray60")
  }

  # Finalize plot
  p <- p +
    facet_grid(rows = vars(Description), scales = "free_y", space = "free_y", switch = "y") +
    scale_fill_manual(values = intensity_colors[levels(dose_df$Intensity)]) +
    labs(
      title = "Dose-Level Configuration Across Candidate Phase II Scenarios",
      x = "Dose (mg)",
      y = NULL,
      caption = "Colors represent dose intensity (Placebo, Low, Mid, High).\nEach point is labeled with its dose."
    ) +
    scale_x_continuous(breaks = pretty(dose_df$Dose)) +
    scale_y_discrete() +
    theme_minimal(base_size = 14) +
    theme(
      strip.text.y.left = element_text(angle = 0, hjust = 0, face = "bold"),
      strip.placement = "outside",
      panel.spacing.y = unit(1, "lines"),
      axis.text.y = if (show_y_axis && !is.null(dose_labels)) element_blank() else element_text(size = 12),
      axis.ticks.y = if (show_y_axis && !is.null(dose_labels)) element_blank() else element_line(),
      legend.position = "bottom",
      legend.title = element_blank()
    )

  return(p)
}
