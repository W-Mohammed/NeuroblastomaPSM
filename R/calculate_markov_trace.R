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
#'   cycle_length = 1/12,
#'   body_weight = 15,
#'   GD2_unit_mg = 20,
#'   GD2_unit_price = 2000,
#'   GD2_dose_days = 10,
#'   TT_unit_mg = 10,
#'   TT_unit_price = 3000,
#'   TT_dose_days = 14,
#'   GD2_aEvents_names = c("fever", "diarrhea", "vomiting", "infection",
#'     "hypersensitivity_reaction", "capillary_leak"),
#'   GD2_aEvents_probs = c(
#'     "fever" = 0.18,
#'     "diarrhea" = 0.07,
#'     "vomiting" = 0.06,
#'     "infection" = 0.21,
#'     "hypersensitivity_reaction" = 0.11,
#'     "capillary_leak" = 0.06
#'   ),
#'   GD2_aEvents_costs = c(
#'     "fever" = 1.98,
#'     "diarrhea" = 3.2,
#'     "vomiting" = 8.45,
#'     "infection" = 222.33,
#'     "hypersensitivity_reaction" = 4.13,
#'     "capillary_leak" = 307.37
#'   ),
#'   Temo_unit_mg = 50,
#'   Temo_unit_price = 2000,
#'   Temo_dose_days = 5,
#'   Iri_unit_mg = 40,
#'   Iri_unit_price = 3000,
#'   Iri_dose_days = 5,
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
