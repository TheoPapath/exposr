#' Simulate Dose-Response Data from a Beta Model
#'
#' Simulates dose-response data using a normalized beta model.
#'
#' @param dose_levels Numeric vector of dose levels.
#' @param n_per_dose Sample size per dose level.
#' @param e0 Baseline effect.
#' @param emax Maximum effect.
#' @param delta1 Left beta shape parameter.
#' @param delta2 Right beta shape parameter.
#' @param scal Normalization factor for dose (max dose should not exceed `scal`).
#' @param sigma Residual error SD.
#' @param dist Distribution for residuals: "normal", "lognormal", "uniform", "t", or "gamma".
#' @param df Degrees of freedom for t-distribution.
#' @param seed Optional random seed.
#'
#' @return Data frame with id, dose, pred, ipred, and scenario.
#'
#' @export
simulate_beta <- function(dose_levels, n_per_dose, e0, emax, delta1, delta2, scal,
                          sigma, dist = "normal", df = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(n_per_dose) == 1) {
    n_per_dose <- rep(n_per_dose, length(dose_levels))
  }
  if (length(n_per_dose) != length(dose_levels)) {
    stop("dose_levels and n_per_dose must be the same length (or provide scalar n_per_dose).")
  }

  xlogx <- function(x) if (x == 0) 0 else x * log(x)

  grid <- expand.grid(emax = emax, delta1 = delta1, delta2 = delta2, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  result_list <- vector("list", length = nrow(grid))

  for (i in seq_len(nrow(grid))) {
    pars <- grid[i, ]
    dose_vector <- rep(dose_levels, times = n_per_dose)
    dose_scaled <- dose_vector / scal
    if (any(dose_scaled > 1)) {
      stop("doses cannot be larger than scal in betaModel")
    }

    logMaxDens <- xlogx(pars$delta1) + xlogx(pars$delta2) - xlogx(pars$delta1 + pars$delta2)
    pred <- e0 + pars$emax / exp(logMaxDens) * (dose_scaled ^ pars$delta1) * (1 - dose_scaled) ^ pars$delta2

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
    label <- paste0("emax=", signif(pars$emax, 3), ", delta1=", signif(pars$delta1, 3), ", delta2=", signif(pars$delta2, 3))

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
