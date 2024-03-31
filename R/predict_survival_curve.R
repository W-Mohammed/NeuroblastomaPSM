#' Predict Cumulative Survival Curve
#'
#' Predict survival probabilities at specified time points from a survival model
#' object, and labels them with the treatment and endpoint information.
#'
#' @param surv_mod A survival model object from which to extract survival
#' probabilities.
#' @param treatment_name Character string specifying the treatment name
#' associated with the survival model.
#' @param end_point Character string indicating the endpoint the survival model
#' is based on.
#' @param time_points Numeric vector of time points at which survival
#' probabilities should be calculated.
#'
#' @return A data frame with survival probabilities at specified time points,
#'         labeled with treatment and end point information.
#' @examples
#' \dontrun{
#' fit <- survival::survfit(
#'     survival::Surv(eventtime, event) ~ 1,
#'     data = IPD_data[IPD_data$trt_cd == "TT",]
#' )
#' survival_curve <- predict_survival_curve(
#'     surv_mod = fit,
#'     treatment_name = "Isotretinoin",
#'     end_point = "OS",
#'     time_points = seq(0, 10, 1/12)
#' )
#' }
#' @export
predict_survival_curve <- function(
    surv_mod,
    treatment_name,
    end_point,
    time_points) {

  surv_summary <- summary(
    object = surv_mod,
    type = "survival",
    t = time_points,
    tidy = FALSE,
    ci = FALSE
  )

  surv_data <- data.frame(
    treatment = treatment_name,
    end_point = end_point,
    time = surv_summary[[1]]$time,
    survival = surv_summary[[1]]$est
  )

  return(surv_data)
}
