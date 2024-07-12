#' Calculate Markov Trace
#'
#' This function calculates the state membership (Markov trace) for different
#' health states based on predicted survival curves.
#'
#' @inheritParams run_psm
#' @param df_survival_curves_long A data frame containing the predicted
#' cumulative survival curves in long format with columns for time, treatment,
#' end_point, and survival probabilities.
#'
#' @return A data frame in wide format with columns for time, treatment, and
#' states occupancy (`EFS`, `PPS`, `D`).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load the fitted Gompertz model parameters
#' models_fit <- NeuroblastomaPSM::parametric_models
#'
#' # Define model parameters
#' params <- c(
#'   time_horizon = 10,
#'   cycle_length = 1/12,
#'   disc_rate_costs = 0.035,
#'   disc_rate_qalys = 0.015,
#'   NeuroblastomaPSM::l_psm_parameters
#' )
#'
#' # Predict cumulative survival
#' df_survival_curves_long <- NeuroblastomaPSM::predict_cumulative_survival(
#'   models_fit = models_fit,
#'   l_params = params
#' )
#'
#' # Generate Markov trace
#' df_markov_trace <- NeuroblastomaPSM::calculate_markov_trace(
#'   df_survival_curves_long = df_survival_curves_long,
#'   l_params = params
#' )
#'
#' rbind(
#'   head(df_markov_trace, n = 5),
#'   tail(df_markov_trace, n = 5)
#' )
#'
#' # Define model parameters - longer time horizons triggers cure_threshold
#' params2 <- c(
#'   time_horizon = 15,
#'   cycle_length = 1/12,
#'   disc_rate_costs = 0.035,
#'   disc_rate_qalys = 0.015,
#'   NeuroblastomaPSM::l_psm_parameters
#' )
#'
#' # Predict cumulative survival
#' df_survival_curves_long2 <- NeuroblastomaPSM::predict_cumulative_survival(
#'   models_fit = models_fit,
#'   l_params = params2
#' )
#'
#' # Generate Markov trace
#' df_markov_trace2 <- NeuroblastomaPSM::calculate_markov_trace(
#'   df_survival_curves_long = df_survival_curves_long2,
#'   l_params = params2
#' )
#'
#' rbind(
#'   head(df_markov_trace2, n = 5),
#'   tail(df_markov_trace2, n = 5)
#' )
#' }
calculate_markov_trace <- function(
    df_survival_curves_long,
    l_params) {
  # Flip survival dataframe to wide format:
  df_survival_curves <- stats::reshape(
    data = df_survival_curves_long,
    timevar = "end_point",
    idvar = c("time", "treatment"),
    direction = "wide"
  )

  # Renaming the columns since reshape() adds a prefix to the column name:
  names(df_survival_curves) <- gsub(
    pattern = "survival\\.",
    replacement = "",
    x = names(df_survival_curves)
  )

  # Calculating 'PPS' and 'D' state occupancy:
  # df_survival_curves$PPS[1:10 * cycle_length] <- df_survival_curves$OS[1:10 * cycle_length] - df_survival_curves$EFS[1:10 * cycle_length]
  # cumprod(df_survival_curves$PPS[length(df_survival_curves$PPS)], adjusted_PPS_cuve)
  df_survival_curves$PPS <- df_survival_curves$OS - df_survival_curves$EFS
  df_survival_curves$D <- 1 - df_survival_curves$OS

  # Calculating 'PPS' and 'D' state occupancy, "cure threshold" assumptions:
  if(l_params$time_horizon > l_params$cure_threshold) {
    # Get parameters
    model_starting_age  <- l_params$age
    curve_starting_age  <- model_starting_age + l_params$cure_threshold
    curve_end_age       <- model_starting_age + l_params$time_horizon
    cycle_length        <- l_params$cycle_length
    cure_threshold      <- l_params$cure_threshold
    cure_starting_cycle <- (cure_threshold / cycle_length) + 1
    v_curve_interpolation_ages <- seq(
      curve_starting_age + cycle_length,
      curve_end_age,
      cycle_length
    )

    # Prepare life table parameters
    v_life_table_ages  <- l_params$df_life_table$age
    v_life_table_porbs <- l_params$df_life_table$probs
    v_life_table_porbs <- v_life_table_porbs[
      v_life_table_ages >= curve_starting_age
    ]
    v_life_table_ages  <- v_life_table_ages[
      v_life_table_ages >= curve_starting_age
    ]

    # Calculate the survival curve for a healthy general population same age cohort
    v_lifeTable_survival_curve <- NeuroblastomaPSM::get_lifeTable_survival_curve(
      mortality_probs = v_life_table_porbs
    )
    # Estimate all-cause mortality for the EFS
    v_cure_EFS_curve <- NeuroblastomaPSM::interpolate_survival_curve(
      survival_curve_ages = v_life_table_ages,
      survival_curve = v_lifeTable_survival_curve,
      target_ages = v_curve_interpolation_ages,
      smr_multiplier = l_params$smr_EFS
    )
    # Estimate all-cause mortality for the PPS
    v_cure_PPS_curve <- NeuroblastomaPSM::interpolate_survival_curve(
      survival_curve_ages = v_life_table_ages,
      survival_curve = v_lifeTable_survival_curve,
      target_ages = v_curve_interpolation_ages,
      smr_multiplier = l_params$smr_PPS
    )
    # Apply the cure threshold assumptions
    df_GD2 <- df_survival_curves[df_survival_curves$treatment == "GD2", ]
    df_GD2$EFS[cure_starting_cycle:nrow(df_GD2)] <- cumprod(
      c(df_GD2$EFS[cure_starting_cycle], v_cure_EFS_curve)
    )
    df_GD2$PPS[cure_starting_cycle:nrow(df_GD2)] <- cumprod(
      c(df_GD2$PPS[cure_starting_cycle], v_cure_PPS_curve)
    )
    df_GD2$D <- 1 - (df_GD2$EFS + df_GD2$PPS)

    df_TT  <- df_survival_curves[df_survival_curves$treatment == "TT", ]
    df_TT$EFS[cure_starting_cycle:nrow(df_TT)] <- cumprod(
      c(df_TT$EFS[cure_starting_cycle], v_cure_EFS_curve)
    )
    df_TT$PPS[cure_starting_cycle:nrow(df_TT)] <- cumprod(
      c(df_TT$PPS[cure_starting_cycle], v_cure_PPS_curve)
    )
    df_TT$D <- 1 - (df_TT$EFS + df_TT$PPS)

    df_survival_curves <- rbind(df_GD2, df_TT)
  }

  # Extract relevant columns:
  df_markov_trace <- df_survival_curves[
    ,
    c("time", "treatment", "EFS", "PPS", "D")
  ]

  # Sanity check:
  stopifnot(
    "Markov trace does not sum up to 1." =
      all(rowSums(df_markov_trace[, c("EFS", "PPS", "D")])  == 1.0000)
  )

  return(df_markov_trace)
}
