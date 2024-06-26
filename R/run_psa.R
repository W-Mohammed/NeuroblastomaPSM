#' Estimate Standard Error from Mean and 95\% Confidence Interval
#'
#' This function estimates the standard error (SE) from the mean and 95\%
#' confidence interval (CI) for normal, beta, and gamma distributions.
#'
#' @param mean A numeric value representing the mean of the distribution.
#' @param lower_ci A numeric value representing the lower bound of the 95\%
#' confidence interval.
#' @param upper_ci A numeric value representing the upper bound of the 95\%
#' confidence interval.
#' @param distribution A character string specifying the type of distribution.
#' Options are "normal", "beta", and "gamma".
#'
#' @return A named list containing the mean and a value representing the
#' estimated standard error (SE), upper and lower bounds of the 95\% confidence
#' interval, and the parameter distribution.
#'
#' @details
#' The standard error (SE) is calculated using different formulas depending on
#' the specified distribution:
#'
#' \describe{
#'   \item{Normal Distribution}{
#'     The margin of error is calculated as \eqn{(upper\_ci - lower\_ci) / 2}.
#'     The standard error is then calculated as:
#'     \deqn{SE = \frac{margin\_of\_error}{1.96}}
#'   }
#'   \item{Beta Distribution}{
#'     First, the parameters \eqn{\alpha} and \eqn{\beta} are estimated using
#'     the method of moments:
#'     \deqn{\alpha = mean \times \left( \frac{mean \times (1 - mean)}
#'     {\left( \frac{margin\_of\_error}{1.96} \right)^2} - 1 \right)}
#'     \deqn{\beta = \alpha \times \left( \frac{1}{mean} - 1 \right)}
#'     The standard error is then calculated as:
#'     \deqn{SE = \sqrt{\frac{\alpha \times \beta}{(\alpha + \beta)^2 \times
#'     (\alpha + \beta + 1)}}}
#'   }
#'   \item{Gamma Distribution}{
#'     First, the shape parameter \eqn{k} and the scale parameter \eqn{\theta}
#'     are estimated:
#'     \deqn{k = \frac{mean^2}{(margin\_of\_error / 1.96)^2}}
#'     \deqn{\theta = \frac{(margin\_of\_error / 1.96)^2}{mean}}
#'     The standard error is then calculated as:
#'     \deqn{SE = \frac{\sqrt{k} \times \theta}{\sqrt{mean}}}
#'   }
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage for normal distribution
#' mean <- 50
#' lower_ci <- 45
#' upper_ci <- 55
#' distribution <- "normal"
#' se <- estimate_se(mean, lower_ci, upper_ci, distribution)
#' print(paste("Estimated Standard Error for", distribution, "distribution:", se))
#'
#' # Example usage for beta distribution
#' mean <- 0.5
#' lower_ci <- 0.4
#' upper_ci <- 0.6
#' distribution <- "beta"
#' se <- estimate_se(mean, lower_ci, upper_ci, distribution)
#' print(paste("Estimated Standard Error for", distribution, "distribution:", se))
#'
#' # Example usage for gamma distribution
#' mean <- 10
#' lower_ci <- 8
#' upper_ci <- 12
#' distribution <- "gamma"
#' se <- estimate_se(mean, lower_ci, upper_ci, distribution)
#' print(paste("Estimated Standard Error for", distribution, "distribution:", se))
#' }
estimate_se <- function(
    mean,
    lower_ci,
    upper_ci,
    distribution) {
  # Estimate SE by distribution
  if (distribution == "normal") {
    # Normal distribution
    margin_of_error <- (upper_ci - lower_ci) / 2
    se <- margin_of_error / 1.96
  } else if (distribution == "beta") {
    # Beta distribution
    margin_of_error <- (upper_ci - lower_ci) / 2
    alpha <- mean * ((mean * (1 - mean) / (margin_of_error / 1.96)^2) - 1)
    beta <- alpha * (1 / mean - 1)
    # Standard error for beta distribution
    se <- sqrt((alpha * beta) / (((alpha + beta)^2) * (alpha + beta + 1)))
  } else if (distribution == "gamma") {
    # Gamma distribution
    margin_of_error <- (upper_ci - lower_ci) / 2
    shape <- mean / (margin_of_error / 1.96)^2
    scale <- mean / shape
    # Standard error for gamma distribution
    se <- sqrt(shape) * scale / sqrt(mean)
  } else {
    stop(
      "Unsupported distribution type. Please use 'normal', 'beta', or 'gamma'."
    )
  }

  return(
    list(
      mean = mean,
      se = se,
      lower_ci = lower_ci,
      upper_ci = upper_ci,
      distribution = distribution
    )
  )
}

#' Estimate Distribution Parameters for rnorm, rgamma, and rbeta
#'
#' This function converts the mean and standard error (SE) into the appropriate
#' parameters for the normal, gamma, and beta distributions.
#' It returns the parameters needed for R's `rnorm()`, `rgamma()`, and `rbeta()`
#' functions.
#'
#' @param mean A numeric value representing the mean of the distribution.
#' @param se A numeric value representing the standard error of the
#' distribution.
#' @param distribution A character string specifying the type of distribution.
#' Options are "normal", "gamma", and "beta".
#'
#' @details
#' The parameters for each distribution are calculated as follows:
#'
#' \describe{
#'   \item{Normal Distribution}{
#'     The parameters for the normal distribution are directly the mean and
#'     standard deviation:
#'     \deqn{mean = \text{mean}}
#'     \deqn{sd = \text{se}}
#'   }
#'   \item{Gamma Distribution}{
#'     The shape (\eqn{\alpha}) and scale (\eqn{\beta}) parameters for the gamma
#'     distribution are calculated using:
#'     \deqn{\alpha = \left( \frac{\text{mean}}{\text{se}} \right)^2}
#'     \deqn{\beta = \frac{\text{se}^2}{\text{mean}}}
#'   }
#'   \item{Beta Distribution}{
#'     The alpha (\eqn{\alpha}) and beta (\eqn{\beta}) parameters for the beta
#'     distribution are calculated using:
#'     \deqn{\text{variance} = \text{se}^2}
#'     \deqn{\text{common\_factor} = \frac{\text{mean} \times (1 -
#'     \text{mean})}{\text{variance}} - 1}
#'     \deqn{\alpha = \text{mean} \times \text{common\_factor}}
#'     \deqn{\beta = (1 - \text{mean}) \times \text{common\_factor}}
#'   }
#' }
#'
#' @return A named list containing the parameters for the specified
#' distribution.
#'
#' \describe{
#'   \item{dist_func}{A scalar (length one vector) naming the distribution.}
#'   \item{dist_params}{A named list with elements for R's sampling functions from
#'   specified distribution:
#'    \describe{
#'      \item{for normal distribution:}{A list with elements `mean` and `sd`.}
#'      \item{for gamma distribution:}{A list with elements `shape` and `scale`.}
#'      \item{for beta distribution:}{A list with elements `alpha` and `beta`.}
#'    }
#'  }
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mean <- 0.5
#' se <- 0.1
#'
#' # Normal distribution parameters
#' normal_params <- get_sampling_params(mean, se, "normal")
#' print(normal_params)
#'
#' # Gamma distribution parameters
#' gamma_params <- get_sampling_params(mean, se, "gamma")
#' print(gamma_params)
#'
#' # Beta distribution parameters
#' beta_params <- get_sampling_params(mean, se, "beta")
#' print(beta_params)
#' }
get_sampling_params <- function(
    mean,
    se,
    distribution) {

  if (distribution == "normal") {

    # For the normal distribution, mean and sd are used directly
    results <- list(
      dist_func = "rnorm",
      dist_params = list(
        mean = mean,
        sd = se
      )
    )

  } else if (distribution == "gamma") {

    # For the gamma distribution, we need to calculate shape and scale
    shape <- (mean / se)^2
    scale <- (se^2) / mean
    results <- list(
      dist_func = "rgamma",
      dist_params = list(
        shape = shape,
        scale = scale
      )
    )

  } else if (distribution == "beta") {

    # For the beta distribution, we need to calculate alpha and beta
    variance <- se^2
    common_factor <- mean * (1 - mean) / variance - 1
    alpha <- mean * common_factor
    beta <- (1 - mean) * common_factor
    results <- list(
      dist_func = "rbeta",
      dist_params = list(
        shape1 = alpha,
        shape2 = beta
      )
    )

  } else if (distribution == "fixed") {

    # For the fixed values
    results <- list(
      dist_func = "fixed",
      dist_params = list(
        value = mean
      )
    )

  } else {
    stop("Unsupported distribution type. Please use 'normal', 'gamma', or
         'beta'.")
  }

  return(results)
}

#' Generate Probabilistic Sensitivity Analysis (PSA) Data
#'
#' This function generates PSA data based on provided parameter names,
#' distributions, and distribution arguments.
#'
#' @inheritParams run_psa
#'
#' @return A data frame where each column represents a parameter, with values
#' sampled according to the specified distributions and arguments.
#'
#' @export
#'
#' @importFrom truncnorm rtruncnorm
#'
#' @examples
#' \dontrun{
#' # Generate PSA data
#' df_psa <- sample_psa_data(
#'  v_psa_params = names(NeuroblastomaPSM::l_psa_parameters),
#'  l_psa_params = NeuroblastomaPSM::l_psa_parameters,
#'  n_sim = 100
#' )
#'
#' head(df_psa)
#'
#' df_psa2 <- sample_psa_data(
#'  v_psa_params = c("u_EFS", "cost_laboratory"),
#'  l_psa_params = NeuroblastomaPSM::l_psa_parameters,
#'  n_sim = 100
#' )
#'
#' head(df_psa2)
#' }
sample_psa_data <- function(
    v_psa_params,
    l_psa_params,
    n_sim) {
  # Initialize an empty list to store the sampled data
  psa_data <- list()

  # Iterate over each parameter to generate the corresponding data
  for (i in seq_along(v_psa_params)) {
    name <- v_psa_params[i]
    dist_func <- l_psa_params[[name]]$dist_func
    dist_args <- l_psa_params[[name]]$dist_params

    if (dist_func == "fixed") {
      fixed_value <- unlist(dist_args)
      # Fixed values are repeated n_sim times
      data <- rep(fixed_value, n_sim)
    } else {
      # Sample from user-defined distribution
      data <- do.call(dist_func, c(list(n = n_sim), dist_args))
    }

    # Store the generated data as a data frame
    psa_data[[name]] <- data
  }

  # Combine all data frames into one
  df_psa <- as.data.frame(psa_data)
  return(df_psa)
}

#' Run Probabilistic Sensitivity Analysis (PSA)
#'
#' This function runs a Probabilistic Sensitivity Analysis (PSA) using a
#' specified model function and sampled parameter values.
#'
#' @param model_func A function to be used for the model, such as the
#' \code{\link{run_psm}}
#' @param model_func_args A list of model arguments and parameters that are not
#' varied in PSA.
#' @param v_psa_params A character vector of PSA parameter names.
#' @param l_psa_params A list containing all model PSA parameters. The list is
#' expected to be either \code{\link{l_psa_parameters}} or of identical
#' structure.
#' @param n_sim An integer specifying the number of simulations to run.
#'
#' @return A data frame where each row represents the results from one
#' simulation of the PSA.
#' @export
#' @examples
#' \dontrun{
#' # Run PSA
#' l_psa_results <- NeuroblastomaPSM::run_psa(
#'   model_func = NeuroblastomaPSM::run_psm,
#'   model_func_args = list(
#'      "models_fit" = NeuroblastomaPSM::parametric_models,
#'      "l_params" = c(
#'      time_horizon = 10,
#'      cycle_length = 1/12,
#'      disc_rate_costs = 0.035,
#'      disc_rate_qalys = 0.015,
#'      NeuroblastomaPSM::l_psm_parameters
#'      )
#'   ),
#'   v_psa_params = names(NeuroblastomaPSM::l_psa_parameters),
#'   l_psa_params = NeuroblastomaPSM::l_psa_parameters,
#'   n_sim = 1e3
#' )
#'
#' View(l_psa_results)
#' View(l_psa_results$m_psa_results)
#' View(l_psa_results$df_psa_samples)
#' }
run_psa <- function(
    model_func = NeuroblastomaPSM::run_psm,
    model_func_args = list(
      "models_fit" = NeuroblastomaPSM::parametric_models,
      "l_params" = c(
        time_horizon = 10,
        cycle_length = 1/12,
        disc_rate_costs = 0.035,
        disc_rate_qalys = 0.015,
        NeuroblastomaPSM::l_psm_parameters
      )
    ),
    v_psa_params = names(NeuroblastomaPSM::l_psa_parameters),
    l_psa_params = NeuroblastomaPSM::l_psa_parameters,
    n_sim = 1e3) {

  # Sample PSA configurations
  df_psa_samples <- sample_psa_data(
    v_psa_params = v_psa_params,
    l_psa_params = l_psa_params,
    n_sim = n_sim
  )

  # model parameters never and not varied in this particular PSA
  xtra_params <- model_func_args$l_params[
    !names(model_func_args$l_params) %in% v_psa_params
  ]

  # Initialize an empty list to store the results
  l_psa_results <- list()

  # Loop through each PSA set:
  for (i in 1:n_sim) {
    # extract the i-th row of PSA parameters
    params <- df_psa_samples[i, , drop = FALSE] |>
      as.list()
    # combine the i-th PSA configuration with the model arguments & other params
    model_func_args$l_params <- c(
      # parameters not varried in PSA
      xtra_params,
      # PSA parameters varied in this particular PSA run
      params
    )

    # Simulate PSA parameter configuration
    run_results <- do.call(
      model_func,
      model_func_args
    )

    # Save results to list
    l_psa_results[[i]] <- run_results
  }

  # Combine all results into one data frame
  m_psa_results <- do.call(rbind, l_psa_results)

  return(
    list(
      m_psa_results = m_psa_results,
      df_psa_samples = df_psa_samples
    )
  )
}
