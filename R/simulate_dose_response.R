#' Simulate Dose-Response Data from a Specified Parametric Model
#'
#' A unified interface to simulate data from various dose-response models.
#' Supports: `"emax"`, `"sigemax"`, `"loglinear"`, `"exponential"`, `"beta"`, `"quadratic"`, `"logistic"`.
#'
#' @param model Model type as a string.
#' @param dose_levels Numeric vector of doses.
#' @param n_per_dose Scalar or vector of subjects per dose.
#' @param sigma Residual error standard deviation.
#' @param dist Residual distribution: `"normal"`, `"lognormal"`, `"uniform"`, `"t"`, `"gamma"`.
#' @param df Degrees of freedom (for `t` distribution).
#' @param seed Optional seed.
#' @param ... Additional model-specific parameters (e.g. `e0`, `emax`, etc.).
#'
#' @return Data frame with simulated data.
#'
#' @examples
#' simulate_dose_response(
#'   model = "sigemax",
#'   dose_levels = c(0, 1, 5, 10),
#'   n_per_dose = 10,
#'   e0 = 0, emax = 1, ed50 = 5, h = 2,
#'   sigma = 0.2, seed = 123
#' )
#'
#' @export
simulate_dose_response <- function(model,
                                   dose_levels,
                                   n_per_dose,
                                   sigma,
                                   dist = "normal",
                                   df = 3,
                                   seed = NULL,
                                   ...) {
  model <- tolower(model)
  if (!model %in% c("emax", "sigemax", "loglinear", "exponential", "beta", "quadratic", "logistic")) {
    stop("Unsupported model type. Choose from: 'emax', 'sigemax', 'loglinear', 'exponential', 'beta', 'quadratic', 'logistic'")
  }
  args <- list(
    dose_levels = dose_levels,
    n_per_dose = n_per_dose,
    sigma = sigma,
    dist = dist,
    df = df,
    seed = seed,
    ...
  )
  switch(
    model,
    emax       = do.call(simulate_emax, args),
    sigemax    = do.call(simulate_sigemax, args),
    loglinear  = do.call(simulate_loglinear, args),
    exponential= do.call(simulate_exponential, args),
    beta       = do.call(simulate_beta, args),
    quadratic  = do.call(simulate_quadratic, args),
    logistic   = do.call(simulate_logistic, args)
  )
}
