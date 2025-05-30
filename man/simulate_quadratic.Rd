% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_quadratic.R
\name{simulate_quadratic}
\alias{simulate_quadratic}
\title{Simulate Dose-Response Data from a Quadratic Model}
\usage{
simulate_quadratic(
  dose_levels,
  n_per_dose,
  e0,
  b1,
  b2,
  sigma,
  dist = "normal",
  df = 3,
  seed = NULL
)
}
\arguments{
\item{dose_levels}{Numeric vector of dose levels.}

\item{n_per_dose}{Either a single integer (equal sample size per dose) or a vector matching `dose_levels`.}

\item{e0}{Baseline effect. Can be a scalar or a vector.}

\item{b1}{Linear coefficient.}

\item{b2}{Quadratic coefficient.}

\item{sigma}{Standard deviation for residual error.}

\item{dist}{Residual error distribution: `"normal"` (default), `"lognormal"`, `"uniform"`, `"t"`, or `"gamma"`.}

\item{df}{Degrees of freedom for t-distribution.}

\item{seed}{Optional seed for reproducibility.}
}
\value{
Data frame with columns: `id`, `dose`, `pred`, `ipred`, `scenario` (combination label).
}
\description{
Simulates data from a quadratic polynomial model:
\deqn{pred = e0 + b1 * dose + b2 * dose^2}
}
\details{
All combinations of `e0`, `b1`, and `b2` will be simulated.
}
\examples{
simulate_quadratic(
  dose_levels = c(0, 1, 5, 10),
  n_per_dose = 10,
  e0 = 0, b1 = 0.5, b2 = -0.02,
  sigma = 0.3,
  dist = "normal",
  seed = 400
)

}
