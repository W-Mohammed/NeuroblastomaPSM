% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_psa.R
\name{get_sampling_params}
\alias{get_sampling_params}
\title{Estimate Distribution Parameters for rnorm, rgamma, and rbeta}
\usage{
get_sampling_params(mean, se, distribution)
}
\arguments{
\item{mean}{A numeric value representing the mean of the distribution.}

\item{se}{A numeric value representing the standard error of the
distribution.}

\item{distribution}{A character string specifying the type of distribution.
Options are "normal", "gamma", and "beta".}
}
\value{
A named list containing the parameters for the specified
distribution.

\describe{
  \item{dist_func}{A scalar (length one vector) naming the distribution.}
  \item{dist_params}{A named list with elements for R's sampling functions from
  specified distribution:
   \describe{
     \item{for normal distribution:}{A list with elements `mean` and `sd`.}
     \item{for gamma distribution:}{A list with elements `shape` and `scale`.}
     \item{for beta distribution:}{A list with elements `alpha` and `beta`.}
   }
 }
}
}
\description{
This function converts the mean and standard error (SE) into the appropriate
parameters for the normal, gamma, and beta distributions.
It returns the parameters needed for R's `rnorm()`, `rgamma()`, and `rbeta()`
functions.
}
\details{
The parameters for each distribution are calculated as follows:

\describe{
  \item{Normal Distribution}{
    The parameters for the normal distribution are directly the mean and
    standard deviation:
    \deqn{mean = \text{mean}}
    \deqn{sd = \text{se}}
  }
  \item{Gamma Distribution}{
    The shape (\eqn{\alpha}) and scale (\eqn{\beta}) parameters for the gamma
    distribution are calculated using:
    \deqn{\alpha = \left( \frac{\text{mean}}{\text{se}} \right)^2}
    \deqn{\beta = \frac{\text{se}^2}{\text{mean}}}
  }
  \item{Beta Distribution}{
    The alpha (\eqn{\alpha}) and beta (\eqn{\beta}) parameters for the beta
    distribution are calculated using:
    \deqn{\text{variance} = \text{se}^2}
    \deqn{\text{common\_factor} = \frac{\text{mean} \times (1 -
    \text{mean})}{\text{variance}} - 1}
    \deqn{\alpha = \text{mean} \times \text{common\_factor}}
    \deqn{\beta = (1 - \text{mean}) \times \text{common\_factor}}
  }
}
}
\examples{
\dontrun{
mean <- 0.5
se <- 0.1

# Normal distribution parameters
normal_params <- get_sampling_params(mean, se, "normal")
print(normal_params)

# Gamma distribution parameters
gamma_params <- get_sampling_params(mean, se, "gamma")
print(gamma_params)

# Beta distribution parameters
beta_params <- get_sampling_params(mean, se, "beta")
print(beta_params)
}
}
