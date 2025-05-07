#' Simulate Dose-Response Data from a Logistic Model
#'
#' Simulates from a logistic dose-response model:
#' \deqn{pred = e0 + emax / (1 + exp(-k * (dose - ed50)))}
#'
#' All combinations of `e0`, `emax`, `ed50`, and `k` will be simulated.
#'
#' @inheritParams simulate_emax
#' @param emax Maximum effect.
#' @param ed50 Dose at 50% of `emax`.
#' @param k Steepness parameter.
#'
#' @return Data frame with columns: `id`, `dose`, `pred`, `ipred`, `scenario`.
#'
#' @examples
#' simulate_logistic(
#'   dose_levels = c(0, 1, 5, 10),
#'   n_per_dose = 10,
#'   e0 = 0, emax = 1, ed50 = 5, k = 1.5,
#'   sigma = 0.2,
#'   dist = "normal",
#'   seed = 500
#' )
#'
#' @export
simulate_logistic <- function(dose_levels, n_per_dose, e0, emax, ed50, k,
                              sigma, dist = "normal", df = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(n_per_dose) == 1) {
    n_per_dose <- rep(n_per_dose, length(dose_levels))
  }
  if (length(n_per_dose) != length(dose_levels)) {
    stop("dose_levels and n_per_dose must be the same length (or provide scalar n_per_dose).")
  }

  grid <- expand.grid(e0 = e0, emax = emax, ed50 = ed50, k = k, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  result_list <- vector("list", length = nrow(grid))

  for (i in seq_len(nrow(grid))) {
    pars <- grid[i, ]
    dose_vector <- rep(dose_levels, times = n_per_dose)
    pred <- pars$e0 + pars$emax / (1 + exp(-pars$k * (dose_vector - pars$ed50)))
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
    label <- paste0("e0=", signif(pars$e0, 3), ", emax=", signif(pars$emax, 3), ", ed50=", signif(pars$ed50, 3), ", k=", signif(pars$k, 3))
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
