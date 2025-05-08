#' Simulate Dose-Response Data from a Sigmoid Emax Model
#'
#' Simulates individual-level responses from a sigmoid Emax dose-response model:
#' \deqn{pred = e0 + emax * dose^h / (ed50^h + dose^h)}
#'
#' All combinations of `e0`, `emax`, `ed50`, and `h` will be simulated.
#'
#' @param dose_levels Numeric vector of dose levels.
#' @param n_per_dose Either a single integer (equal sample size per dose) or a vector matching `dose_levels`.
#' @param e0 Baseline effect. Can be a scalar or a vector.
#' @param emax Maximum drug effect. Can be a scalar or a vector.
#' @param ed50 Dose producing 50% of `emax`. Can be a scalar or a vector.
#' @param h Hill coefficient. Can be a scalar or a vector.
#' @param sigma Standard deviation for residual error.
#' @param dist Residual error distribution: `"normal"` (default), `"lognormal"`, `"uniform"`, `"t"`, or `"gamma"`.
#' @param df Degrees of freedom for t-distribution.
#' @param seed Optional seed for reproducibility.
#'
#' @return Data frame with columns: `id`, `dose`, `pred`, `ipred`, `scenario` (combination label).
#'
#' @export

simulate_sigmoid_emax <- function(dose_levels, n_per_dose, e0, emax, ed50, h, sigma=0,
                                  dist = "normal", df = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(n_per_dose) == 1) {
    n_per_dose <- rep(n_per_dose, length(dose_levels))
  }
  if (length(dose_levels) != length(n_per_dose)) {
    stop("dose_levels and n_per_dose must be the same length (or provide scalar n_per_dose).")
  }

  grid <- expand.grid(e0 = e0, emax = emax, ed50 = ed50, h = h, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  result_list <- vector("list", length = nrow(grid))

  for (i in seq_len(nrow(grid))) {
    pars <- grid[i, ]
    dose_vector <- rep(dose_levels, times = n_per_dose)
    pred <- pars$e0 + pars$emax * dose_vector^pars$h / (pars$ed50^pars$h + dose_vector^pars$h)
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
    label <- paste0("e0=", signif(pars$e0, 3), ", emax=", signif(pars$emax, 3), ", ed50=", signif(pars$ed50, 3), ", h=", signif(pars$h, 3))
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
