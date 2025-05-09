#' Plot Dose-Response with TD and ED Highlighted
#'
#' This function plots a normalized dose-response relationship using an Emax model,
#' highlighting the Target Dose (TD) and Effective Dose (ED) with uncertainty bands.
#'
#' @param e0 Numeric. Placebo effect.
#' @param emax Numeric. Maximum effect over placebo.
#' @param ed50 Numeric. Dose producing 50% of Emax.
#' @param delta Numeric. Target effect over placebo (used to compute TD).
#' @param prop Numeric. Proportion of maximum effect for ED (e.g. 0.9 for 90%).
#' @param max_dose Numeric. Maximum dose to consider.
#' @param sd_uncertainty Numeric. Standard deviation used to simulate uncertainty.
#' @param seed Integer. Seed for reproducibility.
#' @param transform Logical. If TRUE, response is normalized to 0–100%. If FALSE, nominal scale is used.
#' @param show_rect Logical. Whether to display ED/TD rectangles and their annotations.
#' @return A ggplot object.
#' @examples
#' # Basic example with default parameters
#' plot_td_ed()
#'
#' plot_td_ed(x_breaks = c(0, 20, 40, 80, 160))
#'
#' # Example with higher target delta and tighter uncertainty
#' plot_td_ed(delta = 1.5, sd_uncertainty = 3)
#'
#' # Example with extended dose range and lower ED proportion
#' plot_td_ed(max_dose = 300, p = 0.75)
#'
#' # Example without transformation
#' plot_td_ed(transform = FALSE, sd )
#'
#' # Example hiding rectangles
#' plot_td_ed(show_rect = FALSE)
#'
#' @export
plot_td_ed <- function(e0 = 4.5, emax = 1.55, ed50 = 10, delta = 1.0, prop = 0.9,
                       max_dose = 160 + 160/2, sd_uncertainty = 5, seed = 123,
                       transform = TRUE, show_rect = TRUE, x_breaks = NULL) {

  library(ggplot2)

  # Ensure numeric inputs
  if (!is.numeric(prop)) prop <- suppressWarnings(as.numeric(prop))
  if (!is.numeric(delta)) delta <- suppressWarnings(as.numeric(delta))

  if (is.na(prop) || is.na(delta)) {
    stop("Arguments 'p' and 'delta' must be numeric and not NA.")
  }

  dose <- seq(0, max_dose, by = 1)
  raw_response <- e0 + (emax * dose) / (ed50 + dose)
  response <- if (transform) {
    100 * (raw_response - e0) / emax
  } else {
    raw_response
  }

  set.seed(seed)
  lower <- if (transform) pmax(0, response - sd_uncertainty) else response - sd_uncertainty
  upper <- if (transform) pmin(100, response + sd_uncertainty) else response + sd_uncertainty

  df <- data.frame(dose, response, lower, upper)

  target_response <- if (transform) 100 * delta / emax else e0 + delta
  ed_response <- if (transform) 100 * prop else e0 + prop * emax

  td <- min(df$dose[df$response >= target_response])
  ed <- min(df$dose[df$response >= ed_response])

  ed_range <- df$dose[df$lower <= ed_response & df$upper >= ed_response]
  ed_min <- min(ed_range)
  ed_max <- max(ed_range)

  td_range <- df$dose[df$lower <= target_response & df$upper >= target_response]
  td_min <- min(td_range)
  td_max <- max(td_range)

  ed_color <- "#3182bd"
  ed_fill <- "#deebf7"
  td_color <- "#e6550d"
  td_fill <- "#fdd0a2"
  ribbon_color <- "#e0ecf4"

  p <- ggplot(df, aes(x = dose, y = response))

  if (show_rect) {
    p <- p + scale_x_continuous(breaks = if (is.null(x_breaks)) waiver() else x_breaks)
    p <- p +
    scale_y_continuous(breaks = if (transform) seq(0, 100, 20) else waiver()) +
      geom_rect(xmin = ed_min, xmax = ed_max,
                ymin = -Inf, ymax = Inf,
                inherit.aes = FALSE, fill = ed_fill, alpha = 0.3) +
      geom_rect(xmin = td_min, xmax = td_max,
                ymin = -Inf, ymax = Inf,
                inherit.aes = FALSE, fill = td_fill, alpha = 0.3) +
      annotate("text", x = td + 5, y = min(upper), label = paste0("TD = ", td, " [", td_min, ", ", td_max, "]"), color = td_color, hjust = 0) +
      annotate("text", x = ed + 5, y = max(upper)*0.8, label = paste0("ED = ", ed, " [", ed_min, ", ", ed_max, "]"), color = ed_color, hjust = 0)
  }

  p <- p +
    geom_ribbon(aes(ymin = lower, ymax = upper), fill = ribbon_color, alpha = 0.4) +
    geom_line(aes(y = lower), color = "black", size = 0.5) +
    geom_line(aes(y = upper), color = "black", size = 0.5) +
    geom_line(size = 1.2) +
    geom_hline(yintercept = if (transform) 0 else e0, linetype = "dashed", color = "gray40") +
    geom_hline(yintercept = target_response, linetype = "dotted", color = td_color) +
    geom_hline(yintercept = ed_response, linetype = "dotted", color = ed_color) +
    geom_vline(xintercept = td, linetype = "dashed", color = td_color) +
    geom_vline(xintercept = ed, linetype = "dashed", color = ed_color)

  subtitle_text <- if (transform) {
    paste0(
      "TD: Target Dose (TD) is the dose where the normalized response exceeds Δ = delta% increase from placebo.
",
      "ED: Effective Dose (ED) is the dose achieving ", round(100 * prop), "% of the maximum possible effect above placebo."
    )
  } else {
    paste0(
      "TD: Target Dose (TD) is the dose where the response exceeds Δ = ", delta, " units above placebo (e0 = ", e0, ").
",
      "ED: Effective Dose (ED) is the dose achieving a nominal increase of ", prop, " × Emax = ", round(prop * emax, 2), " over placebo."
    )
  }

  caption_text <- if (transform) {
    paste0(
      "Horizontal lines: placebo baseline, target response (Δ = ", delta, "%), and ", round(100 * prop), "% max normalized effect.
",
      "Vertical lines: TD (orange), ED (blue).
",
      "Shaded ribbon: uncertainty around the normalized response.
",
      "Blue box: ED range with uncertainty. Orange box: TD range with uncertainty."
    )
  } else {
    paste0(
      "Horizontal lines: placebo (e0 = ", e0, "), target response (Δ = ", delta, "), and ED level = ", round(e0 + prop * emax, 2), ".
",
      "Vertical lines: TD (orange), ED (blue).
",
      "Shaded ribbon: uncertainty around the nominal-scale response.
",
      "Blue box: ED range with uncertainty. Orange box: TD range with uncertainty."
    )
  }

  p <- p +
    labs(
      title = "Illustration of TD and ED in Dose-Response",
      subtitle = subtitle_text,
      x = "Dose",
      y = if (transform) "Response (% of Maximum Effect)" else "Response (unit)",
      caption = caption_text
    ) +
    theme_minimal(base_size = 14) +
    theme(plot.subtitle = element_text(size = 10))

  if (transform) {
    p + coord_cartesian(ylim = c(0, 100))
  } else {
    p
  }
}
