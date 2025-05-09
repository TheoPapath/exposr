#' rxposr palette for up to 8 groups
#'
#' Returns a character vector of up to 8 hex colors
#'
#' @param n Integer. Number of colors to return (max 8).
#' @return Character vector of hex color codes.
#' @export
exposr_palette_1 <- function(n = 8) {
  base_colors <- c(
    "#0033A0", # navy
    "#ED8B00", # orange
    "#A7A8AA", # grey
    "#78BE20", # green
    "#C6007E", # magenta
    "#00B5E2", # teal
    "#E4002B", # red
    "#FFC72C"  # yellow
  )
  if (n > length(base_colors)) stop("exposr_palette_1 supports up to 8 colors.")
  base_colors[seq_len(n)]
}
