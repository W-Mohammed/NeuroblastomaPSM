#' Perform Economic Analysis
#'
#' This function performs an economic analysis based on the Markov trace and
#' provided cost and utility parameters. It calculates both discounted and
#' un-discounted costs and QALYs for each treatment.
#'
#' @param df_markov_trace A data frame containing the Markov trace with columns
#' for time, treatment, and state occupancies (`EFS`, `PPS`, `D`).
#' @param params A list of model parameters including:
#' \itemize{
#'   \item \code{time_horizon}: The time horizon for the model in years.
#'   \item \code{cycles_per_year}: The number of cycles per year.
#'   \item \code{body_weight}: The body weight of the patient in kilograms.
#'   \item \code{GD2_unit_mg}: The dosage unit for GD2 in milligrams.
#'   \item \code{GD2_unit_price}: The price per unit dosage of GD2.
#'   \item \code{GD2_dose_days}: The number of days GD2 is administered in a
#'   cycle.
#'   \item \code{TT_unit_mg}: The dosage unit for TT in milligrams.
#'   \item \code{TT_unit_price}: The price per unit dosage of TT.
#'   \item \code{TT_dose_days}: The number of days TT is administered in a cycle.
#'   \item \code{c_PPS}: The costs associated with post-progression survival.
#'   \item \code{u_EFS}: The utility associated with event-free survival.
#'   \item \code{u_PPS}: The utility associated with post-progression survival.
#'   \item \code{disc_rate_costs}: The annual discount rate for incurred costs.
#'   \item \code{disc_rate_qalys}: The annual discount rate for accrued QALYs.
#' }
#' @return A list containing two scalars: discounted costs (`v_Dcosts_results`)
#' and discounted QALYs (`v_Dqalys_results`) for each treatment.
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
#'   GD2_unit_mg = 20,
#'   GD2_unit_price = 2000,
#'   GD2_dose_days = 10,
#'   TT_unit_mg = 10,
#'   TT_unit_price = 3000,
#'   TT_dose_days = 14,
#'   c_PPS = 4000,
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
#' # Perform Economic Analysis
#' l_psm_results <- NeuroblastomaPSM::perform_economic_analysis(
#'   df_markov_trace = df_markov_trace,
#'   params = params
#' )
#' }
perform_economic_analysis <- function(
    df_markov_trace,
    params) {
  # Create treatment specific Markov trace matrices (for matrix multiplication)
  m_TR_Isotretinoin <- as.matrix(
    x = df_markov_trace[
      df_markov_trace$treatment == "Isotretinoin", c("EFS", "PPS", "D")]
  )
  m_TR_Dinutuximab_β <- as.matrix(
    x = df_markov_trace[
      df_markov_trace$treatment == "Dinutuximab β", c("EFS", "PPS", "D")]
  )

  # Create vectors of health states payoffs
  v_Util  <- c("EFS" = params$u_EFS, "PPS" = params$u_PPS, "D" = 0)
  l_costs <- calculate_treatment_costs(params = params)

  # Matrix multiplication of trace by:
  ## cost per state per cycle - gives total cost by cycle
  m_costs_Isotretinoin  <- m_TR_Isotretinoin  * l_costs$m_C_Isotretinoin
  m_costs_Dinutuximab_β <- m_TR_Dinutuximab_β * l_costs$m_C_Dinutuximab_β
  ## utility by cycle - gives total utility by cycle
  v_qalys_Isotretinoin  <- m_TR_Isotretinoin %*%
    (v_Util / params$cycles_per_year)
  v_qalys_Dinutuximab_β <- m_TR_Dinutuximab_β %*%
    (v_Util / params$cycles_per_year)

  # Get model cycles time points
  time_points <- seq(
    from = 0,
    to = params$time_horizon,
    by = 1 / params$cycles_per_year
  )

  # Calculate costs and QALYs discount weights
  v_dw_c <- 1 / (1 + params$disc_rate_costs) ^ time_points
  v_dw_e <- 1 / (1 + params$disc_rate_qalys) ^ time_points

  # Prepare un-discounted results
  v_costs_results <- cbind(rowSums(m_costs_Isotretinoin),
                           rowSums(m_costs_Dinutuximab_β)) |>
    `colnames<-`(c("c_Isotretinoin", "c_Dinutuximab_β"))
  v_qalys_results <- cbind(v_qalys_Isotretinoin, v_qalys_Dinutuximab_β) |>
    `colnames<-`(c("q_Isotretinoin", "q_Dinutuximab_β"))

  # Prepare discounted results
  v_Dcosts_results <- v_dw_c %*% v_costs_results |>
    `colnames<-`(c("Isotretinoin", "Dinutuximab_β"))
  v_Dqalys_results <- v_dw_e %*% v_qalys_results |>
    `colnames<-`(c("Isotretinoin", "Dinutuximab_β"))

  return(
    list(
      v_costs_results = colSums(v_costs_results),
      v_sqalys_results = colSums(v_qalys_results),
      v_Dcosts_results = v_Dcosts_results,
      v_Dqalys_results = v_Dqalys_results
    )
  )
}

#' Calculate Treatment Costs
#'
#' This function calculates the treatment costs over a given time horizon, based
#' on the provided model parameters.
#'
#' @param params A list of model parameters including:
#' \itemize{
#'   \item \code{time_horizon}: The time horizon for the model in years.
#'   \item \code{cycles_per_year}: The number of cycles per year.
#'   \item \code{body_weight}: The body weight of the patient in kilograms.
#'   \item \code{GD2_unit_mg}: The dosage unit for GD2 in milligrams.
#'   \item \code{GD2_unit_price}: The price per unit dosage of GD2.
#'   \item \code{GD2_dose_days}: The number of days GD2 is administered in a
#'   cycle.
#'   \item \code{TT_unit_mg}: The dosage unit for TT in milligrams.
#'   \item \code{TT_unit_price}: The price per unit dosage of TT.
#'   \item \code{TT_dose_days}: The number of days TT is administered in a cycle.
#'   \item \code{c_PPS}: The costs associated with post-progression survival.
#'   \item \code{u_EFS}: The utility associated with event-free survival.
#'   \item \code{u_PPS}: The utility associated with post-progression survival.
#'   \item \code{disc_rate_costs}: The annual discount rate for incurred costs.
#'   \item \code{disc_rate_qalys}: The annual discount rate for accrued QALYs.
#' }
#' @param GD2_off_dose_days The number of days GD2 is not administered in a
#' cycle. Default value is `10` days.
#' @param TT_off_dose_days The number of days TT is not administered in a cycle.
#' Default value is `16` days.
#' @return A matrix of treatment costs for the first year, with columns for GD2
#' and TT costs over the specified time points.
#' @examples
#' \dontrun{
#' # Define model parameters
#' params <- list(
#'   time_horizon = 10,
#'   cycles_per_year = 12,
#'   body_weight = 15,
#'   GD2_unit_mg = 20,
#'   GD2_unit_price = 2000,
#'   GD2_dose_days = 10,
#'   TT_unit_mg = 10,
#'   TT_unit_price = 3000,
#'   TT_dose_days = 14,
#'   c_PPS = 4000
#' )
#'
#' # Calculate treatment costs
#' l_treatment_costs <- calculate_treatment_costs(params = params)
#' View(l_treatment_costs)
#' }
calculate_treatment_costs <- function(
    params,
    GD2_off_dose_days = 9,
    TT_off_dose_days = 16) {
  # Get model cycle time points
  time_points <- seq(
    from = 0,
    to = params$time_horizon,
    by = 1 / params$cycles_per_year
  )

  # Calculate body surface area
  surface_area <- (params$body_weight * 0.035) + 0.1

  # Dosage per child per cycle in mg
  v_GD2_dosage_cycle <- rep(# mg GD2/day x bsa x days
    x = (ceiling((100 * surface_area) / params$GD2_unit_mg) *
           params$GD2_unit_price),
    times = params$GD2_dose_days
  )
  v_TT_dosage_cycle  <- rep(# mg  TT/day x bsa x days
    x = (ceiling((160 * surface_area) / params$TT_unit_mg) *
           params$TT_unit_price),
    times = params$TT_dose_days
  )
  v_GD2_cool_off      <- rep(
    x = 0,
    times = GD2_off_dose_days
  )
  v_TT_cool_off       <- rep(
    x = 0,
    times = TT_off_dose_days
  )
  v_annual_GD2_costs  <- c(
    rep(x = c(v_GD2_dosage_cycle, v_TT_dosage_cycle, v_GD2_cool_off), 5),
    v_TT_dosage_cycle, v_GD2_cool_off
  )
  v_annual_TT_costs   <- c(
    rep(x = c(v_TT_dosage_cycle, v_TT_cool_off), 5), v_TT_dosage_cycle
  )
  m_annual_costs <- cbind(
    v_annual_GD2_costs  = c(
      v_annual_GD2_costs,
      rep(0, length.out = 365 - length(v_annual_GD2_costs))
    ),
    v_annual_TT_costs   = c(
      v_annual_TT_costs,
      rep(0, length.out = 365 - length(v_annual_TT_costs))
    ),
    days_in_year = 1:365 / 365
  )

  # Time points overlapping in the first treatment year
  points <- which(time_points <= 1)
  points <- time_points[c(points, points[length(points)] + 1)]

  # Summing costs to the values specified by 'points'
  # Create intervals for days in the year based on 'points'
  intervals <- cut(
    x = m_annual_costs[, "days_in_year"],
    breaks = points,
    include.lowest = TRUE,
    right = FALSE
  )

  # Costs over the time_points
  m_C_Dinutuximab_β <- cbind(
    EFS = c(
      tapply(
        X = m_annual_costs[, "v_annual_GD2_costs"],
        INDEX = intervals,
        FUN = sum
      ) |>
        `rownames<-`(NULL),
      rep(
        x = 0,
        length.out = length(time_points) - length(unique(factor(intervals)))
      )
    ),
    PPS = params$c_PPS,
    D = 0
  )
  m_C_Isotretinoin <- cbind(
    EFS = c(
      tapply(
        X = m_annual_costs[, "v_annual_TT_costs"],
        INDEX = intervals,
        FUN = sum
      ) |>
        `rownames<-`(NULL),
      rep(
        x = 0,
        length.out = length(time_points) - length(unique(factor(intervals)))
      )
    ),
    PPS = params$c_PPS,
    D = 0
  )

  return(
    list(
      m_C_Dinutuximab_β = m_C_Dinutuximab_β,
      m_C_Isotretinoin = m_C_Isotretinoin
    )
  )
}
