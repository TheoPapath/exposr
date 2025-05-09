% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate_emax.R
\name{simulate_emax}
\alias{simulate_emax}
\title{Simulate Dose-Response Data from an Emax Model}
\usage{
simulate_emax(
  dose_levels,
  n_per_dose,
  e0,
  emax,
  ed50,
  sigma = 0,
  dist = "normal",
  df = 3,
  seed = NULL
)
}
\arguments{
\item{dose_levels}{Numeric vector of dose levels.}

\item{n_per_dose}{Either a single integer (equal sample size per dose) or a vector matching `dose_levels`.}

\item{e0}{Baseline effect. Can be a scalar or a vector.}

\item{emax}{Maximum drug effect. Can be a scalar or a vector.}

\item{ed50}{Dose producing 50% of `emax`. Can be a scalar or a vector.}

\item{sigma}{Standard deviation for residual error.}

\item{dist}{Residual error distribution: `"normal"` (default), `"lognormal"`, `"uniform"`, `"t"`, or `"gamma"`.}

\item{df}{Degrees of freedom for t-distribution.}

\item{seed}{Optional seed for reproducibility.}
}
\value{
Data frame with columns: `id`, `dose`, `pred`, `ipred`, `scenario` (combination label).
}
\description{
Simulates individual-level responses from a standard Emax dose-response model:
\deqn{pred = e0 + emax * dose / (ed50 + dose)}
}
\details{
All combinations of `e0`, `emax`, and `ed50` will be simulated.
}
\examples{
simulate_emax(
  dose_levels = c(0, 5, 10, 20),
  n_per_dose = 10,
  e0 = c(0, 0.1), emax = c(1, 1.2), ed50 = c(10, 8),
  sigma = 0.2,
  dist = "normal",
  seed = 123
)

}
