#' Adjust Hazards Underlying a Survival Curve
#'
#' This function compares two survival curves representing a baseline group and
#' a treatment group, and adjusts the hazard rate for the treatment group at and
#' beyond a specified time point to match the hazard rate for the baseline group
#' or to follow specified adjustment mode.
#'
#' @param baseline_survival A numeric vector of survival curve for the baseline
#' group.
#' @param treatment_survival A numeric vector of survival curve for the
#' treatment group. `treatment_survival` is expected to have an equal length to
#' `baseline_survival`.
#' @param adjustment_mode A character string specifying the adjustment mode.
#' Options are:
#'   \itemize{
#'     \item `"baseline_after_time"` (default): Adjust hazard rate to match
#'     baseline at and beyond the specified time point.
#'     \item `"higher_HR_after_time"`: Use the higher hazard rate between
#'     treatment and baseline after the specified time point.
#'     \item `"lower_HR_after_time"`: Use the lower hazard rate between
#'     treatment and baseline after the specified time point.
#'   }
#' @param adjustment_time A numeric value representing the time point at which
#' to start adjusting the hazard rate for the treatment group. Default is `1`.
#' `adjustment_time` expects each element in the `baseline_survival` and
#' `treatment_survival` survival curves (vectors) to represent the proportion of
#' survivors at a time point. The scale or units of the values passed to the
#' `adjustment_time` argument are implicitly equivalent to those corresponding
#' to the survival curves.
#'
#' @return A numeric vector of adjusted hazard rates for the treatment group.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Generate simulated survival data for a baseline and a treatment group
#' set.seed(123)
#' days_in_years <- 1:(365*3)
#' baseline_survival <- 1 - pweibull(q = days_in_years, shape = 1.1, scale = 650)
#' treatment_survival <- 1 - pweibull(q = days_in_years, shape = 4, scale = 900)
#'
#' # Compare the simulated survival data
#' plot(
#'   x = days_in_years/365,
#'   y = baseline_survival,
#'   ylim = c(0, 1),
#'   type = "l",
#'   col = "red",
#'   xlab = "Time in years",
#'   ylab = "Survival"
#'  )
#' lines(x = days_in_years/365, treatment_survival, col = "blue")
#'
#' # Estimate and compare the hazard rates for both groups
#' baseline_hazard <- convert_survival_to_hazard(baseline_survival)
#' treatment_hazard <- convert_survival_to_hazard(treatment_survival)
#'
#' plot(
#'   x = days_in_years/365,
#'   y = baseline_hazard,
#'   ylim = c(min(treatment_hazard), c(max(treatment_hazard))),
#'   type = "l",
#'   col = "red",
#'   xlab = "Time in years",
#'   ylab = "Hazard rate"
#'  )
#' lines(x = days_in_years/365, treatment_hazard, col = "blue")
#'
#' # Adjust the hazard rate for the treatment group to match the hazard rate for
#' # the baseline group at and beyond time point 365
#' adjusted_survival <- NeuroblastomaPSM::adjust_survival_curve(
#'   baseline_survival = baseline_survival,
#'   treatment_survival = treatment_survival,
#'   adjustment_time = 365
#' )
#'
#' plot(
#'   x = days_in_years/365,
#'   y = treatment_survival,
#'   type = "l",
#'   col = "red",
#'   xlab = "Time",
#'   ylab = "Survival"
#' )
#' lines(x = days_in_years/365, y = baseline_survival, col = "blue")
#' lines(x = days_in_years/365, y = adjusted_survival, col = "green")
#'
#' # Adjust the hazard rate for the treatment group to the higher value compared
#' # to the baseline group after time point 365
#' adjusted_survival <- NeuroblastomaPSM::adjust_survival_curve(
#'   baseline_survival = baseline_survival,
#'   treatment_survival = treatment_survival,
#'   adjustment_mode = "higher_HR_after_time",
#'   adjustment_time = 365
#' )
#'
#' plot(
#'   x = days_in_years/365,
#'   y = treatment_survival,
#'   type = "l",
#'   col = "red",
#'   xlab = "Time in years",
#'   ylab = "Survival"
#' )
#' lines(x = days_in_years/365, y = baseline_survival, col = "blue")
#' lines(x = days_in_years/365, y = adjusted_survival, col = "green")
#'
#' # Adjust the hazard rate for the treatment group to the lower value compared
#' # to the baseline group after time point 365
#' adjusted_survival <- NeuroblastomaPSM::adjust_survival_curve(
#'   baseline_survival = baseline_survival,
#'   treatment_survival = treatment_survival,
#'   adjustment_mode = "lower_HR_after_time",
#'   adjustment_time = 365
#' )
#'
#' plot(
#'   x = days_in_years/365,
#'   y = treatment_survival,
#'   type = "l",
#'   col = "red",
#'   xlab = "Time in years",
#'   ylab = "Survival"
#' )
#' lines(x = days_in_years/365, y = baseline_survival, col = "blue")
#' lines(x = days_in_years/365, y = adjusted_survival, col = "green")
#' }
adjust_survival_curve <- function(
    baseline_survival,
    treatment_survival,
    adjustment_mode = "baseline_after_time",
    adjustment_time = 1) {

  # Check both curves are made up of equal number of points
  # Ensure survival_curve is a vector
  if (!identical(length(baseline_survival), length(treatment_survival))) {
    stop("Inputs baseline_survival and treatment_survival should be equal.")
  }

  # Calculate instantaneous hazards for both curves
  baseline_hazard  <- convert_survival_to_hazard(baseline_survival)
  treatment_hazard <- convert_survival_to_hazard(treatment_survival)

  # Define adjustment range
  adjustment_range <- adjustment_time:length(baseline_survival)

  # Adjust based on mode
  switch(
    adjustment_mode,
    "baseline_after_time" = {
      # Hazard in treatment group is equal to baseline after set time
      treatment_hazard[adjustment_range] <- baseline_hazard[adjustment_range]
    },
    "higher_HR_after_time" = {
      # Hazard in treatment group is the higher of the two after set time
      treatment_hazard[adjustment_range] <- pmax(
        baseline_hazard[adjustment_range], treatment_hazard[adjustment_range]
      )
    },
    "lower_HR_after_time" = {
      # Hazard in treatment group is the lower of the two after set time
      treatment_hazard[adjustment_range] <- pmin(
        baseline_hazard[adjustment_range], treatment_hazard[adjustment_range]
      )
    }
  )

  # Convert the adjusted hazard rates back to survival curve
  adjusted_survival_curve <- convert_hazard_to_survival(treatment_hazard)

  # Return the updated survival curve for the treatment group
  return(adjusted_survival_curve)
}

#' Convert Survival Curve to Instantaneous Hazard Rates
#'
#' This function converts a numeric vector of survival probabilities into
#' instantaneous hazard rates. The relationship between survival probabilities
#' and the discrete hazard rates is given by:
#'
#' \deqn{h(t_i) = -\log\left(\frac{S(t_i)}{S(t_{i-1})}\right)}
#'
#' where \eqn{S(t_i)} is the survival probability at time \eqn{t_i} and
#' \eqn{h(t_i)} is the hazard rate at time \eqn{t_i}.
#'
#' @param survival_curve A numeric vector representing survival probabilities.
#'
#' @return A numeric vector of instantaneous hazard rates.
#' @export
#'
#' @examples
#' \dontrun{
#' survival_curve <- c(0.99, 0.95, 0.90, 0.85)
#'
#' v_hazard_rates <- NeuroblastomaPSM::convert_survival_to_hazard(
#' survival_curve = survival_curve
#' )
#'
#' v_hazard_rates
#' }
convert_survival_to_hazard <- function(survival_curve) {

  v_hazard_rate <- -log(survival_curve /
                          c(1, survival_curve[1:(length(survival_curve) - 1)]))

  return(v_hazard_rate)
}

#' Convert Instantaneous Hazard Rates to Survival Curve
#'
#' This function converts a vector of hazard rates into a survival curve.
#' The survival probability at each time point is calculated based on the
#' cumulative sum of the hazard rates up to that point.
#'
#' The relationship between the hazard rates and the survival probabilities is
#' given by:
#' \deqn{S(t) = \exp(-\sum_{i=1}^{t} h_i)}
#' where \eqn{S(t)} is the survival probability at time \eqn{t}, and \eqn{h_i}
#' is the hazard rate at time \eqn{i}.
#'
#' @param hazard_rates A numeric vector of hazard rates.
#'
#' @return A numeric vector of survival probabilities corresponding to the input
#' hazard rates.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example hazard rates
#' hazard_rates <- c(0.01, 0.02, 0.015, 0.03)
#'
#' # Convert hazard rates to survival probabilities
#' survival_curve <- convert_hazard_to_survival(hazard_rates)
#'
#' survival_curve
#' }
convert_hazard_to_survival <- function(hazard_rates) {

  survival_curve <- exp(-cumsum(hazard_rates))

  return(survival_curve)
}

#' Convert Hazard Rate to Probability
#'
#' This function converts a hazard rate to the probability of an event occurring
#' within a specified time unit. The conversion uses the exponential
#' relationship between hazard rates and survival probabilities.
#'
#' The relationship is given by the formula:
#' \deqn{p = 1 - \exp(-h \cdot \Delta t)}
#' where \eqn{p} is the probability of the event occurring within the time unit,
#' \eqn{h} is the hazard rate, and \eqn{\Delta t} is the length of the time
#' unit.
#'
#' This formula assumes a constant hazard rate over the specified time period.
#'
#' @param v_hazard_rate A numeric value or vector representing the hazard rates.
#' @param time_unit A numeric value representing the length of the time unit
#' over which the hazard rate is applied. Default is 1.
#'
#' @return A numeric value or vector of probabilities corresponding to the input
#' hazard rates.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example hazard rate (per year)
#' hazard_rate <- 0.05
#'
#' # Time unit in years
#' time_unit <- 1
#'
#' # Convert hazard rate to probability
#' event_probability <- convert_hazard_to_probability(hazard_rate, time_unit)
#'
#' event_probability
#'
#' # Example with a vector of hazard rates
#' hazard_rates <- c(0.01, 0.02, 0.03)
#' event_probabilities <- convert_hazard_to_probability(hazard_rates, time_unit)
#'
#' event_probabilities
#' }
convert_hazard_to_probability <- function(v_hazard_rate, time_unit = 1) {

  v_probability <- 1 - exp(-v_hazard_rate * time_unit)

  return(v_probability)
}

#' Interpolate Survival Curve for a Range of Ages
#'
#' This function fits a regression model to interpolate (calculate) the
#' probability of dying for a range of user-specified ages. The fitted model
#' regresses the cumulative hazard rate, which is transformed from the input
#' survival function (curve), on the corresponding age.
#' The cumulative hazard (\eqn{H(t)}) is related to the survival function
#' (\eqn{S(t)}) by:
#' \deqn{H(t) = -\log(S(t))}
#' where \eqn{S(t)} is the survival probability up to time \eqn{t}.
#'
#' @param survival_curve_ages A numeric vector representing the ages corresponding to the
#' survival probabilities.
#' @param survival_curve A numeric vector of mortality probabilities (the
#' probability of dying) corresponding to `age`.
#' @param target_ages A numeric vector of ages for which the hazard rates are to
#' be predicted.
#' @param interpolation_model A character string specifying the model to use for
#' fitting the log cumulative hazard. Options are "gam" (default) for
#' Generalized Additive Model and "spline" for spline model.
#' @param smr_multiplier A numeric value representing the Standardized Mortality
#' Ratio (SMR) multiplier. The SMR is used to adjust the mortality rates of the
#' population under study compared to a standard population. An SMR of 1
#' (default) indicates no difference in mortality rates, while an SMR greater
#' than 1 indicates higher mortality in the study population.
#'
#' @return A numeric vector of the interpolated survival function (curve).
#' ages.
#'
#' @importFrom mgcv s
#' @export
#'
#' @examples
#' \dontrun{
#' # Define age range and survival probabilities
#' model_starting_age <- NeuroblastomaPSM::l_psm_parameters$age
#' df_lifeTable <- NeuroblastomaPSM::df_lifeTable_Jordan
#' age <- df_lifeTable$`Age Group`
#' mortality_probs <- df_lifeTable$`Both sexes`
#' mortality_probs <- mortality_probs[age >= model_starting_age]
#' age <- age[age >= model_starting_age]
#'
#' # Calculate the survival curve for the modeled cohort
#' survival_curve <- NeuroblastomaPSM::get_lifeTable_survival_curve(
#'    mortality_probs = mortality_probs
#' )
#'
#' # Define output ages
#' target_ages <- seq(model_starting_age, 29, 7/365)
#'
#' # Interpolate Survival Curve
#' v_interpolated_curve <- NeuroblastomaPSM::interpolate_survival_curve(
#'   survival_curve_ages = age,
#'   survival_curve = survival_curve,
#'   target_ages = target_ages
#' )
#'
#' v_interpolated_curve
#'
#' # Define output ages
#' target_ages2 <- seq(min(age), max(age), 7/365)
#'
#' # Interpolate Survival Curve
#' set.seed(1)
#' v_interpolated_curve2 <- NeuroblastomaPSM::interpolate_survival_curve(
#'   survival_curve_ages = age,
#'   survival_curve = survival_curve,
#'   target_ages = target_ages2
#' )
#'
#' v_interpolated_curve2
#'
#' # Compare fitted and observed survival curves
#' plot(x = age, y = survival_curve, xlab = "Age", ylab = "Survival")
#' lines(x = target_ages2, y = v_interpolated_curve2, col = "blue")
#'
#' # Interpolate Survival Curve with a SMR = 5.6
#' set.seed(1)
#' v_interpolated_curve3 <- NeuroblastomaPSM::interpolate_survival_curve(
#'   survival_curve_ages = age,
#'   survival_curve = survival_curve,
#'   target_ages = target_ages2,
#'   smr_multiplier = NeuroblastomaPSM::l_psm_parameters$smr_EFS
#' )
#'
#' v_interpolated_curve3
#'
#' # Compare fitted and observed survival curves
#' plot(x = age, y = survival_curve, xlab = "Age", ylab = "Survival")
#' lines(x = target_ages2, y = v_interpolated_curve2, col = "blue")
#' lines(x = target_ages2, y = v_interpolated_curve3, col = "orange")
#'
#' # Interpolate Survival Curve with a SMR = 5.6 * 1.9
#' set.seed(1)
#' v_interpolated_curve4 <- NeuroblastomaPSM::interpolate_survival_curve(
#'   survival_curve_ages = age,
#'   survival_curve = survival_curve,
#'   target_ages = target_ages2,
#'   smr_multiplier = NeuroblastomaPSM::l_psm_parameters$smr_PPS
#' )
#'
#' v_interpolated_curve4
#'
#' # Compare fitted and observed survival curves
#' plot(x = age, y = survival_curve, xlab = "Age", ylab = "Survival")
#' lines(x = target_ages2, y = v_interpolated_curve2, col = "blue")
#' lines(x = target_ages2, y = v_interpolated_curve3, col = "orange")
#' lines(x = target_ages2, y = v_interpolated_curve4, col = "red")
#' }
interpolate_survival_curve <- function(
    survival_curve_ages,
    survival_curve,
    target_ages,
    interpolation_model = "gam",
    smr_multiplier = 1) {

  # Estimate cumulative hazards
  df_cum_hazard <- data.frame(
    age    = survival_curve_ages,
    cum_hazard = -log(survival_curve)
  )

  # Fit interpolation model: log cumulative hazard vs. age using splines or gam
  cum_hazard_model <- switch (interpolation_model,
    spline = stats::lm(
      formula = log(cum_hazard) ~ splines::ns(x = age, df = 4),
      data = df_cum_hazard
    ),
    gam = mgcv::gam(log(cum_hazard) ~ s(age), data = df_cum_hazard)
  )

  # Predict hazard rate using the fitted model coefficients
  v_fitted_log_cum_hazard <- stats::predict(
    object = cum_hazard_model,
    newdata = data.frame(age = target_ages)
  )
  v_fitted_cum_hazard_rate <- exp(v_fitted_log_cum_hazard)

  # Adjust the hazard rates of the population under study using SMR multiplier
  v_fitted_cum_hazard_rate <- v_fitted_cum_hazard_rate * smr_multiplier

  # Convert the predicted cumulative hazard rates to survival curve
  v_survival_curve <- exp(-v_fitted_cum_hazard_rate)

  return(v_survival_curve)
}

#' Calculate Survival Curve from Mortality Probabilities
#'
#' This function calculates the survival curve from a given vector of mortality
#' probabilities. The survival curve represents the probability of surviving up
#' to each age or time point, starting from a cohort with an initial size of 1
#' (or 100% survival).
#'
#' The survival curve (\eqn{S(t)}) is calculated as the cumulative product of
#' (1 - mortality probability) for each time period.
#'
#' \deqn{S(t) = \prod_{i=1}^{t} (1 - q_i)}
#'
#' where \eqn{q_i} is the mortality probability at time \eqn{i}.
#'
#' @param mortality_probs A numeric vector representing the mortality
#' probabilities for each age or time point.
#'
#' @return A numeric vector representing the survival probabilities at each age
#' or time point.
#' @export
#'
#' @examples
#' \dontrun{
#' # Define a vector of mortality probabilities
#' mortality_probs <- df_lifeTable$`Both sexes`
#'
#' # Calculate the survival curve
#' survival_curve <- NeuroblastomaPSM::get_lifeTable_survival_curve(
#'    mortality_probs = mortality_probs
#' )
#'
#' survival_curve
#' plot(survival_curve, type = 'l')
#' }
get_lifeTable_survival_curve <- function(mortality_probs) {

  v_survival_curve <- cumprod(1 - mortality_probs)

  return(v_survival_curve)
}

#' Estimate Mortality Probabilities from a Survival Curve
#'
#' This function estimates the mortality probabilities from a given cumulative
#' survival curve. The survival curve represents the probability of beyond each
#' time point or age.
#'
#' @param survival_curve A numeric vector representing the survival curve
#' corresponding to each age.
#'
#' @return A numeric vector of estimated mortality probabilities corresponding
#' to each age.
#' @export
#'
#' @examples
#' \dontrun{
#' mortality_probs <- c(0.01, 0.02, 0.03, 0.04)
#'
#' survival_curve <- get_lifeTable_survival_curve(mortality_probs)
#'
#' estimated_mortality_probs <- get_lifeTable_mortality_probs(survival_curve)
#'
#' estimated_mortality_probs
#' }
get_lifeTable_mortality_probs <- function(survival_curve) {
  # Ensure survival_curve is a vector
  if (!is.vector(survival_curve)) {
    stop("Input survival_curve should be a vector.")
  }

  # Calculate the mortality probabilities
  v_mortality_probs <- c(
    1 - survival_curve[1],
    1 - survival_curve[-1] / survival_curve[-length(survival_curve)]
  )

  return(v_mortality_probs)
}
