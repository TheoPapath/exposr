#' Simulate Dose-Response Data from a Log-Linear Model
#'
#' Simulates from a log-linear dose-response model:
#' \deqn{pred = e0 + delta * log(dose + log_shift)}
#'
#' All combinations of `e0`, `delta`, and `log_shift` will be simulated.
#'
#' @inheritParams simulate_emax
#' @param delta Slope of the log-linear relationship.
#' @param log_shift Value added inside the log to avoid log(0). Default is 1.
#'
#' @return Data frame with columns: `id`, `dose`, `pred`, `ipred`, `scenario`.
#'
#' @examples
#' simulate_loglinear(
#'   dose_levels = c(0, 1, 5, 10),
#'   n_per_dose = 10,
#'   e0 = c(0, 0.1), delta = c(1.5, 1.8), log_shift = c(0.5, 1),
#'   sigma = 0.2,
#'   dist = "normal",
#'   seed = 100
#' )
#'
#' @export
simulate_loglinear <- function(dose_levels, n_per_dose, e0, delta, log_shift = 1,
                               sigma, dist = "normal", df = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(n_per_dose) == 1) {
    n_per_dose <- rep(n_per_dose, length(dose_levels))
  }
  if (length(n_per_dose) != length(dose_levels)) {
    stop("dose_levels and n_per_dose must be the same length (or provide scalar n_per_dose).")
  }

  grid <- expand.grid(e0 = e0, delta = delta, log_shift = log_shift, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  result_list <- vector("list", length = nrow(grid))

  for (i in seq_len(nrow(grid))) {
    pars <- grid[i, ]
    dose_vector <- rep(dose_levels, times = n_per_dose)
    pred <- pars$e0 + pars$delta * log(dose_vector + pars$log_shift)
    n <- length(pred)
    residual <- switch(
      tolower(dist),
      "normal"    = rnorm(n, mean = 0, sd = sigma),
      "lognormal" = rlnorm(n, meanlog = 0, sdlog = sigma) - exp(sigma^2 / 2),
      "uniform"   = runif(n, min = -sigma * sqrt(3), max = sigma * sqrt(3)),
      "t"         = rt(n, df = df) * (sigma / sqrt(df / (df - 2))),
      "gamma"     = rgamma(n, shape = (1 / sigma)^2, scale = sigma^2) - (1 / sigma),
      stop("Unsupported distribution: choose from 'normal', 'lognormal', 'uniform', 't', 'gamma")
    )
    ipred <- pred + residual
    label <- paste0("e0=", signif(pars$e0, 3), ", delta=", signif(pars$delta, 3), ", log_shift=", signif(pars$log_shift, 3))
    result_list[[i]] <- data.frame(
      id = seq_along(dose_vector),
      dose = dose_vector,
      pred = pred,
      ipred = ipred,
      scenario = label
    )
  }

  do.call(rbind, result_list)
}
