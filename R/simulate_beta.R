#' Simulate Dose-Response Data from a Beta Model
#'
#' Simulates dose-response data using a normalized beta model, as implemented in the function:
#' \code{betaMod <- function(dose, e0, eMax, delta1, delta2, scal)}.
#' The model computes the response as:
#'
#' \deqn{f(d) = e0 + \frac{eMax}{\exp(\delta_1 \log(\delta_1) + \delta_2 \log(\delta_2) - (\delta_1 + \delta_2) \log(\delta_1 + \delta_2))} \cdot \left(\frac{d}{\text{scal}}\right)^{\delta_1} (1 - \frac{d}{\text{scal}})^{\delta_2}}
#'
#' where \eqn{d} is the administered dose, \eqn{e0} is the baseline effect, and \eqn{scal} is a scaling constant.
#'
#' @param dose_levels Numeric vector of dose levels to simulate.
#' @param n_per_dose Either a single integer (for equal sample size per dose) or a numeric vector of same length as `dose_levels`.
#' @param e0 Baseline effect. Scalar.
#' @param eMax Maximum effect size. Can be a scalar or vector to simulate multiple scenarios.
#' @param delta1 Left shape parameter of the beta function. Can be scalar or vector.
#' @param delta2 Right shape parameter of the beta function. Can be scalar or vector.
#' @param scal Positive numeric value used to normalize dose (max dose must not exceed `scal`).
#' @param sigma Standard deviation of the residual error.
#' @param dist Distribution for the residual error: one of "normal", "lognormal", "uniform", "t", or "gamma".
#' @param df Degrees of freedom for the t-distribution (only used if `dist = "t"`).
#' @param seed Optional integer to set the random seed for reproducibility.
#'
#' @return A data frame with the following columns:
#' \describe{
#'   \item{id}{Subject identifier}
#'   \item{dose}{Administered dose (before scaling)}
#'   \item{pred}{Predicted value from the beta model}
#'   \item{ipred}{Individual predicted value after adding residual error}
#'   \item{scenario}{Label for parameter combination used in the simulation}
#' }
#'
#' @examples
#' sim_data <- simulate_beta(
#'   dose_levels = c(0, 5, 10, 20),
#'   n_per_dose = 10,
#'   e0 = 0,
#'   eMax = 1,
#'   delta1 = 2,
#'   delta2 = 4,
#'   scal = 20,
#'   sigma = 0.1,
#'   dist = "normal",
#'   seed = 123
#' )
#' head(sim_data)
#'
#' @export
simulate_beta <- function(dose_levels, n_per_dose, e0, eMax, delta1, delta2, scal,
                          sigma, dist = "normal", df = 3, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (length(n_per_dose) == 1) {
    n_per_dose <- rep(n_per_dose, length(dose_levels))
  }
  if (length(n_per_dose) != length(dose_levels)) {
    stop("dose_levels and n_per_dose must be the same length (or provide scalar n_per_dose).")
  }

  xlogx <- function(x) if (x == 0) 0 else x * log(x)

  grid <- expand.grid(eMax = eMax, delta1 = delta1, delta2 = delta2, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  result_list <- vector("list", length = nrow(grid))

  for (i in seq_len(nrow(grid))) {
    pars <- grid[i, ]
    dose_vector <- rep(dose_levels, times = n_per_dose)
    dose_scaled <- dose_vector / scal
    if (any(dose_scaled > 1)) {
      stop("doses cannot be larger than scal in betaModel")
    }

    logMaxDens <- xlogx(pars$delta1) + xlogx(pars$delta2) - xlogx(pars$delta1 + pars$delta2)
    pred <- e0 + pars$eMax / exp(logMaxDens) * (dose_scaled ^ pars$delta1) * (1 - dose_scaled) ^ pars$delta2

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
    label <- paste0("eMax=", signif(pars$eMax, 3), ", delta1=", signif(pars$delta1, 3), ", delta2=", signif(pars$delta2, 3))

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
