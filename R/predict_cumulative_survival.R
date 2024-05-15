#' Predict Cumulative Survival
#'
#' This function predicts cumulative survival curves using fitted survival
#' models over a specified time horizon.
#'
#' @param models_fit A named list of fitted survival models.
#' @param params A list of model parameters including:
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
#' @return A data frame containing the predicted cumulative survival curves with
#' columns for time, treatment, and survival probabilities for different
#' endpoints.
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
#' # Predict cumulative survival
#' df_survival_curves_long <- NeuroblastomaPSM::predict_cumulative_survival(
#'   models_fit = models_fit,
#'   params = params
#' )
#'
#' rbind(
#'   head(df_survival_curves_long, n = 5),
#'   tail(df_survival_curves_long, n = 5)
#' )
#' }
predict_cumulative_survival <- function(models_fit, params) {
  time_points <- seq(
    from = 0,
    to = params$time_horizon,
    by = 1 / params$cycles_per_year
  )

  df_survival_curves_long <- lapply(
    X = names(models_fit) |>
      `names<-`(names(models_fit)),
    FUN = function(curve_nm) {
      model_fit <- models_fit[[curve_nm]]
      end_point <- ifelse(
        test = grepl(pattern = "OS", x = curve_nm),
        yes = "OS",
        no = "EFS"
      )
      treatment_nm <- gsub(
        pattern = paste0(".", end_point),
        replacement = "",
        x = curve_nm
      )

      NeuroblastomaPSM::predict_survival_curve(
        surv_mod = model_fit,
        treatment_name = treatment_nm,
        end_point = end_point,
        time_points = time_points
      )
    }) |>
    do.call(
      what = rbind,
      args = _
    ) |>
    `rownames<-`(NULL)

  return(df_survival_curves_long)
}
