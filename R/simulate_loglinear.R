#' Simulate Dose-Response Data from a Log-Linear Model
#'
#' Simulates from a log-linear dose-response model:
#' \deqn{pred = e0 + e1 * log(dose + log_shift)}
#'
#' @inheritParams simulate_emax
#' @param e1 Slope of the log-linear relationship.
#' @param log_shift Value added inside the log to avoid log(0). Default is 1.
#'
#' @return Data frame with columns: `id`, `dose`, `pred`, `ipred`.
#'
#' @examples
#' simulate_loglinear(
#'   dose_levels = c(0, 1, 5, 10),
#'   n_per_dose = 10,
#'   e0 = 0, e1 = 1.5, log_shift = 1,
#'   sigma = 0.2,
#'   dist = "normal",
#'   seed = 100
#' )
#'
#' @export
simulate_loglinear <- function(dose_levels, n_per_dose, e0, e1, log_shift = 1,
                               sigma, dist = "normal", df = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(n_per_dose) == 1) {
    n_per_dose <- rep(n_per_dose, length(dose_levels))
  }
  if (length(n_per_dose) != length(dose_levels)) {
    stop("dose_levels and n_per_dose must be the same length (or provide scalar n_per_dose).")
  }
  dose_vector <- rep(dose_levels, times = n_per_dose)
  pred <- e0 + e1 * log(dose_vector + log_shift)
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
