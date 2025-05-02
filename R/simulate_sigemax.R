#' Simulate Dose-Response Data from a Sigmoid Emax Model
#'
#' Simulates data from a sigmoid Emax model with Hill coefficient:
#' \deqn{pred = e0 + emax * dose^h / (ed50^h + dose^h)}
#'
#' @inheritParams simulate_emax
#' @param h Hill coefficient controlling the curve steepness.
#'
#' @return Data frame with columns: `id`, `dose`, `pred`, `ipred`.
#'
#' @examples
#' simulate_sigemax(
#'   dose_levels = c(0, 1, 5, 10),
#'   n_per_dose = 10,
#'   e0 = 0, emax = 1, ed50 = 5, h = 2,
#'   sigma = 0.2,
#'   dist = "normal",
#'   seed = 42
#' )
#'
#' @export
simulate_sigemax <- function(dose_levels, n_per_dose, e0, emax, ed50, h,
                             sigma, dist = "normal", df = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(n_per_dose) == 1) {
    n_per_dose <- rep(n_per_dose, length(dose_levels))
  }
  if (length(n_per_dose) != length(dose_levels)) {
    stop("dose_levels and n_per_dose must be the same length (or provide scalar n_per_dose).")
  }
  dose_vector <- rep(dose_levels, times = n_per_dose)
  pred <- e0 + emax * dose_vector^h / (ed50^h + dose_vector^h)
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
