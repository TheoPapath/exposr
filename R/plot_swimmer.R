#' Plot Swimmer Plot
#'
#' Creates a swimmer plot to visualize duration-based individual patient data.
#' Each row typically represents a subject, showing time on treatment (start to end),
#' along with optional event indicators (e.g., progression), censoring markers,
#' biomarker values over time, and annotations. Useful for pharmacometric studies
#' to visualize individual-level timelines annotated by exposure or response.
#'
#' @param data A data frame containing the swimmer data.
#' @param id_var Column name for subject ID.
#' @param start_var Column name for start time (default = 0 if missing).
#' @param end_var Column name for end time.
#' @param event_var Optional; column name for binary or categorical event indicator (e.g., progression).
#' @param event_time_var Optional; column name for time of event (e.g., progression time).
#' @param censor_time_var Optional; column name for time of censoring/dropout.
#' @param biomarker_time_var Optional; column name for time of biomarker measurement.
#' @param biomarker_value_var Optional; column name for biomarker values to be plotted as points.
#' @param exposure_var Optional; continuous variable for exposure (e.g., AUC, Cmax).
#' @param color_by Optional; column name to color bars by (e.g., treatment group or binned exposure).
#' @param sort_by Optional; variable name to sort subjects by (e.g., exposure, duration).
#' @param facet_by Optional; variable name to facet by (e.g., treatment group).
#' @param title Optional title.
#' @param x_lab Optional label for x-axis.
#' @param y_lab Optional label for y-axis.
#' @param show_grid Logical; whether to show horizontal grid lines (default = TRUE).
#' @param annotate_var Optional; column name with text to annotate on bars (e.g., exposure, response).
#'
#' @return A ggplot swimmer plot object.
#' @examples
#' if (interactive()) {
#'   set.seed(123)
#'   df <- data.frame(
#'     id = paste0("P", 1:10),
#'     start = 0,
#'     end = sample(30:90, 10),
#'     event_time = c(20, NA, 45, 80, 33, NA, 52, NA, NA, 38),
#'     event = c(1, 0, 1, 1, 1, 0, 1, 0, 0, 1),
#'     censor_time = c(NA, 65, NA, NA, NA, 72, NA, NA, 58, NA),
#'     AUC = round(runif(10, 100, 500), 1),
#'     best_resp = sample(c("CR", "PR", "SD", "PD"), 10, replace = TRUE),
#'     group = rep(c("A", "B"), each = 5)
#'   )
#'   df$ExposureBin <- cut(df$AUC, breaks = quantile(df$AUC, probs = seq(0, 1, 0.25)), include.lowest = TRUE)
#'
#'   biomarkers <- expand.grid(id = df$id, time = c(10, 20, 40)) |>
#'     dplyr::left_join(df[, c("id", "group")], by = "id") |>
#'     dplyr::mutate(biomarker = runif(n(), 10, 100))
#'
#'   df_biomarker <- dplyr::left_join(df, biomarkers, by = "id")
#'
#'   plot_swimmer(
#'     data = df_biomarker,
#'     id_var = "id",
#'     start_var = "start",
#'     end_var = "end",
#'     event_var = "event",
#'     event_time_var = "event_time",
#'     censor_time_var = "censor_time",
#'     biomarker_time_var = "time",
#'     biomarker_value_var = "biomarker",
#'     exposure_var = "AUC",
#'     color_by = "ExposureBin",
#'     annotate_var = "best_resp",
#'     sort_by = "AUC",
#'     facet_by = "group",
#'     title = "Example Swimmer Plot with Exposure and Biomarkers"
#'   )
#' }
#'
#' @export
plot_swimmer <- function(
    data,
    id_var,
    start_var = NULL,
    end_var,
    event_var = NULL,
    event_time_var = NULL,
    censor_time_var = NULL,
    biomarker_time_var = NULL,
    biomarker_value_var = NULL,
    exposure_var = NULL,
    color_by = NULL,
    sort_by = NULL,
    facet_by = NULL,
    title = "Swimmer Plot",
    x_lab = "Time",
    y_lab = "Subjects",
    show_grid = TRUE,
    annotate_var = NULL
) {
  library(ggplot2)
  library(dplyr)

  data <- data %>% mutate(ID = .data[[id_var]])
  if (!is.null(facet_by) && facet_by %in% names(data)) {
    data[[facet_by]] <- as.factor(data[[facet_by]])
  }

  if (is.null(start_var)) {
    data$start <- 0
  } else {
    data$start <- data[[start_var]]
  }
  data$end <- data[[end_var]]

  if (!is.null(color_by)) {
    data$color <- as.factor(data[[color_by]])
    color_scale <- scale_color_brewer(palette = "Set2")
  } else if (!is.null(exposure_var)) {
    data$color <- cut(
      data[[exposure_var]],
      breaks = quantile(data[[exposure_var]], probs = seq(0, 1, length.out = 5 + 1), na.rm = TRUE),
      include.lowest = TRUE
    )
    color_scale <- scale_color_gradient(low = "#deebf7", high = "#08519c")
  } else {
    data$color <- NA
    color_scale <- NULL
  }

  if (is.null(sort_by)) {
    data <- data %>% mutate(.duration = end - start) %>% arrange(.duration)
  } else {
    data <- data %>% arrange(.data[[sort_by]])
  }

  data <- data %>% mutate(ID = factor(ID, levels = rev(unique(ID))))

  p <- ggplot(data) +
    geom_segment(aes(x = start, xend = end, y = ID, yend = ID, color = color), linewidth = 3, lineend = "round")

  if (!is.null(event_time_var) && !is.null(event_var)) {
    data$event_time <- data[[event_time_var]]
    data$event <- data[[event_var]]
    event_data <- data %>% filter(!is.na(event_time) & event == 1)
    if (!is.null(facet_by) && facet_by %in% names(event_data)) {
      event_data <- event_data %>% select(ID, event_time, all_of(facet_by))
    } else {
      event_data <- event_data %>% select(ID, event_time)
    }
    p <- p + geom_point(data = event_data, mapping = aes(x = event_time, y = ID),
                        shape = 21, fill = "black", color = "white", size = 3, stroke = 0.5)
  }

  if (!is.null(censor_time_var)) {
    data$censor_time <- data[[censor_time_var]]
    censor_data <- data %>% filter(!is.na(censor_time))
    if (!is.null(facet_by) && facet_by %in% names(censor_data)) {
      censor_data <- censor_data %>% select(ID, censor_time, all_of(facet_by))
    } else {
      censor_data <- censor_data %>% select(ID, censor_time)
    }
    p <- p + geom_point(data = censor_data, mapping = aes(x = censor_time, y = ID),
                        shape = 4, size = 3, stroke = 1, color = "red")
  }

  if (!is.null(biomarker_time_var) && !is.null(biomarker_value_var)) {
    data$biomarker_time <- data[[biomarker_time_var]]
    data$biomarker_val <- data[[biomarker_value_var]]
    biomarker_data <- data %>% filter(!is.na(biomarker_time) & !is.na(biomarker_val))
    if (!is.null(facet_by) && facet_by %in% names(biomarker_data)) {
      biomarker_data <- biomarker_data %>% select(ID, biomarker_time, biomarker_val, all_of(facet_by))
    } else {
      biomarker_data <- biomarker_data %>% select(ID, biomarker_time, biomarker_val)
    }
    p <- p + geom_point(data = biomarker_data, mapping = aes(x = biomarker_time, y = ID, size = biomarker_val),
                        shape = 22, fill = "blue", color = "black", alpha = 0.5)
  }

  if (!is.null(annotate_var)) {
    data$label <- as.character(data[[annotate_var]])
    label_data <- data %>% filter(!is.na(label))
    if (!is.null(facet_by) && facet_by %in% names(label_data)) {
      label_data <- label_data %>% select(ID, end, label, all_of(facet_by))
    } else {
      label_data <- label_data %>% select(ID, end, label)
    }
    p <- p + geom_text(data = label_data, mapping = aes(x = end, y = ID, label = label),
                       hjust = -0.1, size = 3.2, color = "black")
  }

  if (!is.null(color_scale)) {
    p <- p + color_scale
  }

  p <- p +
    theme_minimal() +
    labs(
      title = title,
      x = x_lab,
      y = y_lab,
      color = if (!is.null(color_by)) color_by else if (!is.null(exposure_var)) paste("Binned", exposure_var) else NULL,
      caption = "Bars = time on treatment; Circles = events; X = censoring; Squares = biomarker."
    ) +
    theme(
      panel.grid.major.y = if (show_grid) element_line() else element_blank(),
      panel.grid.minor.y = element_blank(),
      axis.text.y = element_text(size = 8)
    )



  print(p)

  return(p)
}
