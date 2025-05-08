#' Simulate Dose-Response Data from a Quadratic Model
#'
#' Simulates data from a quadratic polynomial model:
#' \deqn{pred = e0 + b1 * dose + b2 * dose^2}
#'
#' All combinations of `e0`, `b1`, and `b2` will be simulated.
#'
#' @inheritParams simulate_emax
#' @param b1 Linear coefficient.
#' @param b2 Quadratic coefficient.
#'
#' @return Data frame with columns: `id`, `dose`, `pred`, `ipred`, `scenario` (combination label).
#'
#' @examples
#' simulate_quadratic(
#'   dose_levels = c(0, 1, 5, 10),
#'   n_per_dose = 10,
#'   e0 = 0, b1 = 0.5, b2 = -0.02,
#'   sigma = 0.3,
#'   dist = "normal",
#'   seed = 400
#' )
#'
#' @export
simulate_quadratic <- function(dose_levels, n_per_dose, e0, b1, b2,
                               sigma, dist = "normal", df = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(n_per_dose) == 1) {
    n_per_dose <- rep(n_per_dose, length(dose_levels))
  }
  if (length(n_per_dose) != length(dose_levels)) {
    stop("dose_levels and n_per_dose must be the same length (or provide scalar n_per_dose).")
  }

  grid <- expand.grid(e0 = e0, b1 = b1, b2 = b2, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  result_list <- vector("list", length = nrow(grid))

  for (i in seq_len(nrow(grid))) {
    pars <- grid[i, ]
    dose_vector <- rep(dose_levels, times = n_per_dose)
    pred <- pars$e0 + pars$b1 * dose_vector + pars$b2 * dose_vector^2
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
    label <- paste0("e0=", signif(pars$e0, 3), ", b1=", signif(pars$b1, 3), ", b2=", signif(pars$b2, 3))
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
