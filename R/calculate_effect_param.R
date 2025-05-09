#' Calculate Emax or equivalent parameter given a target effect, ED50, and dose
#'
#' Supports standard Emax, sigmoid Emax, log-linear, quadratic, and betaMod models.
#'
#' @param delta Numeric. Target effect (e.g. from Target Medicine Profile)
#' @param ed50 Numeric. Estimated ED50 of the drug (ignored for log-linear and quadratic)
#' @param dose Numeric. Dose that leads to the target effect
#' @param model Character. Model type: "emax", "sigmoid", "loglin", "quadratic", or "betamod"
#' @param hill Numeric. Hill coefficient (only used for sigmoid model)
#' @param delta1 Numeric. Shape parameter 1 for betaMod (optional)
#' @param delta2 Numeric. Shape parameter 2 for betaMod (optional)
#' @param scal Numeric. Scaling factor for betaMod (e.g. max dose)
#' @param e0 Numeric. Baseline effect (default is 0)
#' @return Numeric. Estimated Emax or model-specific parameter
#' @examples
#' calculate_effect_param(delta = 1.5, ed50 = 10, dose = 160, model = "emax")
#' calculate_effect_param(delta = 1.5, ed50 = 10, dose = 160, model = "sigmoid", hill = 2)
#' calculate_effect_param(delta = 1.5, ed50 = NA, dose = 160, model = "loglin")
#' calculate_effect_param(delta = 1.5, ed50 = NA, dose = 160, model = "quadratic")
#' calculate_effect_param(delta = 1.5, ed50 = NA, dose = 160, model = "betamod", delta1 = 1.2, delta2 = 2, scal = 200)
calculate_effect_param <- function(delta, ed50, dose,
                                   model = c("emax", "sigmoid", "loglin", "quadratic", "betamod"),
                                   hill = 1, delta1 = NULL, delta2 = NULL, scal = NULL, e0 = 0) {
  model <- match.arg(model)
  net_delta <- delta - e0

  if (model == "emax") {
    param <- (net_delta * (ed50 + dose)) / dose

  } else if (model == "sigmoid") {
    param <- (net_delta * (ed50^hill + dose^hill)) / (dose^hill)

  } else if (model == "loglin") {
    if (dose <= 0) stop("Dose must be positive for log-linear model.")
    param <- net_delta / log(dose)

  } else if (model == "quadratic") {
    # Quadratic: delta = e0 + b1 * dose + b2 * dose^2, assuming b1 = 0
    param <- net_delta / (dose^2)

  } else if (model == "betamod") {
    if (is.null(delta1) || is.null(delta2) || is.null(scal)) {
      stop("delta1, delta2, and scal must be provided for betaMod model.")
    }
    xlogx <- function(x) ifelse(x == 0, 0, x * log(x))
    logMaxDens <- xlogx(delta1) + xlogx(delta2) - xlogx(delta1 + delta2)
    norm_dose <- dose / scal
    shape <- (norm_dose^delta1) * (1 - norm_dose)^delta2
    param <- net_delta * exp(logMaxDens) / shape
  }

  return(param)
}
