#' Run Partitioned Survival Model (PSM)
#'
#' This function runs the Partitioned Survival Model (PSM) that estimates the
#' cost-effectiveness of 'Isotretinoin' vs 'Dinutuximab β' in the treatment of
#' Neuroblastoma in children.
#'
#' @param models_fit A named list of fitted survival models.
#' @param l_params A list of model parameters including:
#' \itemize{
#'   \item \code{time_horizon}: The time horizon for the model in years.
#'   \item \code{cycles_per_year}: The number of cycles per year.
#'   \item \code{body_weight}: The body weight of the patient in kilograms.
#'   \item \code{unit_GD2_mg}: The dosage unit for GD2 in milligrams.
#'   \item \code{unit_GD2_price}: The price per unit dosage of GD2.
#'   \item \code{unit_TT_mg}: The dosage unit for TT in milligrams.
#'   \item \code{unit_TT_price}: The price per unit dosage of TT.
#'   \item \code{c_PPS}: The costs associated with post-progression survival.
#'   \item \code{u_EFS}: The utility associated with event-free survival.
#'   \item \code{u_PPS}: The utility associated with post-progression survival.
#'   \item \code{disc_rate_costs}: The annual discount rate for incurred costs.
#'   \item \code{disc_rate_qalys}: The annual discount rate for accrued QALYs.
#' }
#' @export
#' @return A list containing two scalars: discounted costs (`v_Dcosts_results`)
#' and discounted QALYs (`v_Dqalys_results`) for each treatment.
#' @examples
#' \dontrun{
#' # Load the fitted Gompertz model parameters
#' models_fit <- NeuroblastomaPSM::parametric_models
#'
#' # Define model parameters
#' params <- list(
#'   time_horizon = 10,
#'   cycles_per_year = 12,
#'   body_weight = 15,
#'   unit_GD2_mg = 20,
#'   unit_GD2_price = 2000,
#'   unit_TT_mg = 10,
#'   unit_TT_price = 3000,
#'   c_PPS = 3200,
#'   u_EFS = 0.23,
#'   u_PPS = 0.23,
#'   disc_rate_costs = 0.035,
#'   disc_rate_qalys = 0.015
#' )
#'
#' # Run the Partitioned Survival Model
#' l_psm_results <- NeuroblastomaPSM::run_psm(
#'   models_fit = NeuroblastomaPSM::parametric_models,
#'   l_params = params
#' )
#' # Run the Partitioned Survival Model
#' l_psm_results <- NeuroblastomaPSM::run_psm(
#'   l_params = params
#' )
#' }
run_psm <- function(
    models_fit = NeuroblastomaPSM::parametric_models,
    l_params) {
  # Predict cumulative survival
  df_survival_curves_long <- NeuroblastomaPSM::predict_cumulative_survival(
    models_fit = models_fit,
    params = l_params
  )

  # Generate Markov trace
  df_markov_trace <- NeuroblastomaPSM::calculate_markov_trace(
    df_survival_curves_long = df_survival_curves_long
  )

  # Perform Economic Analysis
  l_PSM_results <- NeuroblastomaPSM::perform_economic_analysis(
    df_markov_trace = df_markov_trace,
    params = l_params
  )

  return(l_PSM_results)
}
