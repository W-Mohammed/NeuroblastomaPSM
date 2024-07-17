#' Run Partitioned Survival Model (PSM)
#'
#' This function runs the Partitioned Survival Model (PSM) that estimates the
#' cost-effectiveness of 'Isotretinoin' vs 'Dinutuximab β' in the treatment of
#' Neuroblastoma in children.
#'
#' @param models_fit A named list of fitted survival models.
#' @param l_params A list of model parameters including the list of parameters
#' in the list \code{\link{l_psm_parameters}} in addition to:
#' \itemize{
#'   \item \code{time_horizon}: The time horizon for the model in years.
#'   \item \code{cycle_length}: The length of a model cycle measured in years.
#'   \item \code{disc_rate_costs}: The annual discount rate for incurred costs.
#'   \item \code{disc_rate_qalys}: The annual discount rate for accrued QALYs.
#' }
#'
#' @return A vector containing two scalars: discounted costs and discounted
#' QALYs for each treatment.
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
#'   time_horizon = 15,
#'   cycle_length = 1/12,
#'   disc_rate_costs = 0.035,
#'   disc_rate_qalys = 0.015,
#'   NeuroblastomaPSM::l_psm_parameters
#' )
#'
#' # Run the Partitioned Survival Model
#' v_psm_results <- NeuroblastomaPSM::run_psm(
#'   models_fit = NeuroblastomaPSM::parametric_models,
#'   l_params = params
#' )
#'
#' v_psm_results
#'
#' # Run the Partitioned Survival Model
#' v_psm_results <- NeuroblastomaPSM::run_psm(
#'   l_params = params
#' )
#'
#' v_psm_results
#' }
run_psm <- function(
    models_fit = NeuroblastomaPSM::parametric_models,
    l_params) {
  # Predict cumulative survival
  df_survival_curves_long <- NeuroblastomaPSM::predict_cumulative_survival(
    models_fit = models_fit,
    l_params = l_params
  )

  # Generate Markov trace
  df_markov_trace <- NeuroblastomaPSM::calculate_markov_trace(
    df_survival_curves_long = df_survival_curves_long,
    l_params = l_params
  )

  # Perform Economic Analysis
  v_PSM_results <- NeuroblastomaPSM::perform_economic_analysis(
    df_markov_trace = df_markov_trace,
    l_params = l_params
  )

  return(v_PSM_results)
}
