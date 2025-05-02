#' Simulate Dose-Response Data from an Emax Model
#'
#' Simulates individual-level responses from a standard Emax dose-response model:
#' \deqn{pred = e0 + emax * dose / (ed50 + dose)}
#'
#' @param dose_levels Numeric vector of dose levels.
#' @param n_per_dose Either a single integer (equal sample size per dose) or a vector matching `dose_levels`.
#' @param e0 Baseline effect.
#' @param emax Maximum drug effect.
#' @param ed50 Dose producing 50% of `emax`.
#' @param sigma Standard deviation for residual error.
#' @param dist Residual error distribution: `"normal"` (default), `"lognormal"`, `"uniform"`, `"t"`, or `"gamma"`.
#' @param df Degrees of freedom for t-distribution.
#' @param seed Optional seed for reproducibility.
#'
#' @return Data frame with columns: `id`, `dose`, `pred`, `ipred`.
#'
#' @examples
#' simulate_emax(
#'   dose_levels = c(0, 5, 10, 20),
#'   n_per_dose = 10,
#'   e0 = 0, emax = 1, ed50 = 10,
#'   sigma = 0.2,
#'   dist = "normal",
#'   seed = 123
#' )
#'
#' @export

simulate_emax <- function(dose_levels, n_per_dose, e0, emax, ed50, sigma,
                          dist = "normal", df = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(n_per_dose) == 1) {
    n_per_dose <- rep(n_per_dose, length(dose_levels))
  }
  if (length(dose_levels) != length(n_per_dose)) {
    stop("dose_levels and n_per_dose must be the same length (or provide scalar n_per_dose).")
  }
  dose_vector <- rep(dose_levels, times = n_per_dose)
  pred <- e0 + emax * dose_vector / (ed50 + dose_vector)
  n <- length(pred)
  residual <- switch(
    tolower(dist),
    "normal"    = rnorm(n, mean = 0, sd = sigma),
    "lognormal" = rlnorm(n, meanlog = 0, sdlog = sigma) - exp(sigma^2 / 2),
    "uniform"   = runif(n, min = -sigma * sqrt(3), max = sigma * sqrt(3)),
    "t"         = rt(n, df = df) * (sigma / sqrt(df / (df - 2))),
    "gamma"     = rgamma(n, shape = (1 / sigma)^2, scale = sigma^2) - (1 / sigma),
    stop("Unsupported distribution: choose from 'normal', 'lognormal', 'uniform', 't', 'gamma'")
  )
  ipred <- pred + residual
  data.frame(id = seq_along(dose_vector), dose = dose_vector, pred = pred, ipred = ipred)
}
