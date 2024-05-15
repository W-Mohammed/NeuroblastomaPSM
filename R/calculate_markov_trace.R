#' Calculate Markov Trace
#'
#' This function calculates the state membership (Markov trace) for different
#' health states based on predicted survival curves.
#'
#' @param df_survival_curves_long A data frame containing the predicted
#' cumulative survival curves in long format with columns for time, treatment,
#' end_point, and survival probabilities.
#' @return A data frame in wide format with columns for time, treatment, and
#' states occupancy (`EFS`, `PPS`, `D`).
#' @export
#' @examples
#' \dontrun{
#' # Load the fitted Gompertz model parameters
#' models_fit <- NeuroblastomaPSM::parametric_models
#'
#' # Define model parameters
#' params <- list(
#'   time_horizon = 10,
#'   cycles_per_year = 12,
#'   c_TT  = 2000,
#'   c_GD2 = 3000,
#'   c_PPS = 3200,
#'   u_EFS = 0.23,
#'   u_PPS = 0.23,
#'   disc_rate_costs = 0.035,
#'   disc_rate_qalys = 0.015
#' )
#'
#' # Predict cumulative survival
#' df_survival_curves_long <- NeuroblastomaPSM::predict_cumulative_survival(
#'   models_fit = models_fit,
#'   params = params
#' )
#'
#' # Generate Markov trace
#' df_markov_trace <- NeuroblastomaPSM::calculate_markov_trace(
#'   df_survival_curves_long = df_survival_curves_long
#' )
#'
#' rbind(
#'   head(df_markov_trace, n = 5),
#'   tail(df_markov_trace, n = 5)
#' )
#' }
calculate_markov_trace <- function(df_survival_curves_long) {
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
  df_survival_curves$PPS <- df_survival_curves$OS - df_survival_curves$EFS
  df_survival_curves$D <- 1 - df_survival_curves$OS

  # Extract relevant columns:
  df_markov_trace <- df_survival_curves[
    ,
    c("time", "treatment", "EFS", "PPS", "D")
  ]

  # Sanity check:
  stopifnot(
    "Markov trace does not sum up to 1." =
      all(rowSums(df_markov_trace[, c("EFS", "PPS", "D")]) == 1))

  return(df_markov_trace)
}
