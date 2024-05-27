#' Predict Cumulative Survival
#'
#' This function predicts cumulative survival curves using fitted survival
#' models over a specified time horizon.
#'
#' @inheritParams run_psm
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
#'   params = params
#' )
#'
#' rbind(
#'   head(df_survival_curves_long, n = 5),
#'   tail(df_survival_curves_long, n = 5)
#' )
#' }
predict_cumulative_survival <- function(
    models_fit,
    params) {
  time_points <- seq(
    from = 0,
    to = params$time_horizon,
    by = params$cycle_length
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
