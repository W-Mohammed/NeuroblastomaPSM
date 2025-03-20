#' Perform Economic Analysis
#'
#' This function performs an economic analysis based on the Markov trace and
#' provided cost and utility parameters. It calculates both discounted and
#' un-discounted costs and QALYs for each treatment.
#'
#' @param df_markov_trace A data frame containing the Markov trace with columns
#' for time, treatment, and state occupancies (`EFS`, `PPS`, `D`).
#' @inheritParams run_psm
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
#' # Perform Economic Analysis
#' v_psm_results <- NeuroblastomaPSM::perform_economic_analysis(
#'   df_markov_trace = df_markov_trace,
#'   l_params = params
#' )
#'
#' v_psm_results
#' }
perform_economic_analysis <- function(
    df_markov_trace,
    l_params) {
  # Create treatment specific Markov trace matrices (for matrix multiplication)
  m_TR_TT <- as.matrix(
    x = df_markov_trace[
      df_markov_trace$treatment == "TT", c("EFS", "PPS", "D")]
  )
  m_TR_GD2 <- as.matrix(
    x = df_markov_trace[
      df_markov_trace$treatment == "GD2", c("EFS", "PPS", "D")]
  )

  # Create vectors of health states payoffs
  v_Util  <- c("EFS" = l_params$u_EFS, "PPS" = l_params$u_PPS, "D" = 0)
  l_costs <- calculate_treatment_costs(l_params = l_params)

  # Matrix multiplication of trace by:
  ## cost per state per cycle - gives total cost by cycle
  m_costs_TT  <- m_TR_TT  * l_costs$m_C_TT
  m_costs_GD2 <- m_TR_GD2 * l_costs$m_C_GD2
  ## utility by cycle - gives total utility by cycle
  v_qalys_TT  <- m_TR_TT %*%
    (v_Util * l_params$cycle_length)
  v_qalys_GD2 <- m_TR_GD2 %*%
    (v_Util * l_params$cycle_length)

  # Get model cycles time points
  time_points <- seq(
    from = 0,
    to = l_params$time_horizon,
    by = l_params$cycle_length
  )

  # Calculate costs and QALYs discount weights
  v_dw_c <- 1 / (1 + l_params$disc_rate_costs) ^ time_points
  v_dw_e <- 1 / (1 + l_params$disc_rate_qalys) ^ time_points

  # Prepare un-discounted results
  v_costs_results <- cbind(rowSums(m_costs_TT),
                           rowSums(m_costs_GD2)) |>
    `colnames<-`(c("costs_TT", "costs_GD2"))
  v_qalys_results <- cbind(v_qalys_TT, v_qalys_GD2) |>
    `colnames<-`(c("qalys_TT", "qalys_GD2"))

  # Prepare discounted results
  v_Dcosts_results <- v_dw_c %*% v_costs_results |>
    `colnames<-`(c("Dcosts_TT", "Dcosts_GD2")) |>
    _[1, ]
  v_Dqalys_results <- v_dw_e %*% v_qalys_results |>
    `colnames<-`(c("Dqalys_TT", "Dqalys_GD2")) |>
    _[1, ]

  return(
    c(
      colSums(v_costs_results),
      colSums(v_qalys_results),
      v_Dcosts_results,
      v_Dqalys_results
    )
  )
}

#' Calculate Treatment Costs
#'
#' This function calculates the treatment costs, , `Dinutuximab β` (GD2) and
#' `Isotretinoin` (TT). including the costs of managing any associated adverse
#' events, over a given time horizon.
#'
#' @inheritParams perform_economic_analysis
#' @param GD2_cycle_days The number of days in a GD2 treatment cycle. Default
#' value is `35` days.
#' @param TT_cycle_days The number of days in a TT treatment cycle. Default
#' value is `30` days.
#' @param Temo_cycle_days The number of days in a Temozolomide treatment cycle.
#' Default value is `21` days.
#' @param Iri_cycle_days The number of days in a Irinotecan treatment cycle.
#' Default value is `21` days.
#'
#' @return A matrix of treatment costs for the first year, with columns for GD2
#' and TT costs over the specified time points.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define model parameters
#' params <- c(
#'   time_horizon = 10,
#'   cycle_length = 1/12,
#'   disc_rate_costs = 0.035,
#'   disc_rate_qalys = 0.015,
#'   NeuroblastomaPSM::l_psm_parameters
#' )
#'
#' # Calculate treatment costs
#' l_treatment_costs <- NeuroblastomaPSM::calculate_treatment_costs(
#'    l_params = params
#' )
#'
#' View(l_treatment_costs)
#' }
calculate_treatment_costs <- function(
    l_params,
    GD2_cycle_days = 35,
    TT_cycle_days = 30,
    Temo_cycle_days = 21,
    Iri_cycle_days = 21) {
  # Estimate EFS costs
  l_efs_costs <- calculate_efs_costs(
    l_params = l_params,
    GD2_cycle_days = GD2_cycle_days,
    TT_cycle_days = TT_cycle_days
  )

  # Estimate PPS costs
  v_pps_costs <- calculate_pps_costs(
    l_params = l_params,
    Temo_cycle_days = Temo_cycle_days,
    Iri_cycle_days = Iri_cycle_days
  )

  # Get intervention groups costs together
  m_C_GD2 <- cbind(
    EFS = l_efs_costs$GD2_EFS_costs,
    PPS = v_pps_costs,
    D = 0
  )
  m_C_TT <- cbind(
    EFS = l_efs_costs$TT_EFS_costs,
    PPS = v_pps_costs,
    D = 0
  )

  return(
    list(
      m_C_GD2 = m_C_GD2,
      m_C_TT = m_C_TT
    )
  )
}

#' Calculate Event Free Survival (EFS) Costs
#'
#' This function calculates the costs associated with the Neuroblastoma
#' interventions, `Dinutuximab β` (GD2) and `Isotretinoin` (TT).
#'
#' @inheritParams calculate_treatment_costs
#'
#' @return A list containing `Dinutuximab β` (GD2) and `Isotretinoin` (TT) costs
#' over the model time-horizon.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define model parameters
#' params <- c(
#'   time_horizon = 10,
#'   cycle_length = 1/12,
#'   disc_rate_costs = 0.035,
#'   disc_rate_qalys = 0.015,
#'   NeuroblastomaPSM::l_psm_parameters
#' )
#'
#' # Calculate AE treatment costs
#' l_GD2_efs_costs <- NeuroblastomaPSM::calculate_efs_costs(
#'  l_params = params,
#'  GD2_cycle_days = 35,
#'  TT_cycle_days = 28
#' )
#'
#' l_GD2_efs_costs
#' }
calculate_efs_costs <- function(
    l_params,
    GD2_cycle_days,
    TT_cycle_days) {
  # Calculate days off treatments
  GD2_off_dose_days <- GD2_cycle_days - (l_params$GD2_dose_days +
                                           l_params$TT_dose_days)
  TT_off_dose_days <- TT_cycle_days - l_params$TT_dose_days
  # Estimate GD2 AE costs per cycle length
  GD2_AE_costs_cycle <- calculate_ae_costs(l_params = l_params)
  GD2_AE_costs_day <- GD2_AE_costs_cycle / (l_params$cycle_length * 365)

  # Get model cycle time points
  time_points <- seq(
    from = 0,
    to = l_params$time_horizon,
    by = l_params$cycle_length
  )

  # Calculate body surface area
  surface_area <- (l_params$body_weight * 0.035) + 0.1

  # Dosage per child per cycle in mg
  v_GD2_dosage_cycle <- rep(# mg GD2/day x bsa x days
    x = (ceiling((20 * surface_area) / l_params$GD2_unit_mg) *
           l_params$GD2_unit_price),
    times = l_params$GD2_dose_days
  )
  v_TT_dosage_cycle  <- rep(# mg  TT/day x bsa x days
    x = (ceiling((160 * surface_area) / l_params$TT_unit_mg) *
           l_params$TT_unit_price),
    times = l_params$TT_dose_days
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

    v_TT_dosage_cycle      , # adding possible AE costs
    v_TT_cool_off         ,  # adding possible AE costs

    rep(
      x = c(
        v_GD2_dosage_cycle + GD2_AE_costs_day # adding possible AE costs and cost of hospitalization
                           + l_params$cost_hospitalisation,
        v_TT_dosage_cycle  + GD2_AE_costs_day, # adding possible AE costs
        v_GD2_cool_off     + GD2_AE_costs_day  # adding possible AE costs
      ),
      times = 5
    )

  )
  v_annual_TT_costs   <- c(
    rep(
      x = c(
        v_TT_dosage_cycle,
        v_TT_cool_off
      ),
      times = 6
    )
  )
  m_annual_costs <- cbind(
    v_annual_GD2_costs  = c(
      v_annual_GD2_costs,
      rep(0, length.out = (1 + 365) - length(v_annual_GD2_costs))
    ),
    v_annual_TT_costs   = c(
      v_annual_TT_costs,
      rep(0, length.out = (1 + 365) - length(v_annual_TT_costs))
    ),
    days_in_year = 0:365 / 365
  )

  # Time points overlapping in the first treatment year
  points <- which(time_points <= 1)
  points <- time_points[c(points, points[length(points)] + 1)]
  points <- points[!is.na(points)]
  stopifnot("Error in length of time!" = length(points) <= length(time_points))

  # Summing costs to the values specified by 'points'
  # Create intervals for days in the year based on 'points'
  intervals <- cut(
    x = m_annual_costs[, "days_in_year"],
    breaks = points,
    include.lowest = TRUE,
    right = FALSE
  )

  # Costs over the time_points
  GD2_EFS_costs <- c(
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
  )

  TT_EFS_costs <- c(
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
  )

  return(
    list(
      "GD2_EFS_costs" = GD2_EFS_costs,
      "TT_EFS_costs" = TT_EFS_costs
    )
  )
}

#' Calculate Adverse Effects (AE) Costs
#'
#' This function calculates the costs associated with the adverse effects of
#' receiving `Dinutuximab β` (GD2) over a given time horizon, based on the
#' provided model parameters.
#'
#' @inheritParams calculate_treatment_costs
#'
#' @return A scalar representing the costs of related to `Dinutuximab β` (GD2)
#' adverse events.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define model parameters
#' params <- c(
#'   time_horizon = 10,
#'   cycle_length = 1/12,
#'   disc_rate_costs = 0.035,
#'   disc_rate_qalys = 0.015,
#'   NeuroblastomaPSM::l_psm_parameters
#' )
#'
#' # Calculate AE costs
#' GD2_ae_costs <- NeuroblastomaPSM::calculate_ae_costs(l_params = params)
#'
#' GD2_ae_costs
#' }
calculate_ae_costs <- function(l_params) {
  # Get model cycle time points
  time_points <- seq(
    from = 0,
    to = l_params$time_horizon,
    by = l_params$cycle_length
  )

  # Combine GD2 AE probs and costs
  AE_data <- l_params[
    grepl(
      pattern = paste0(l_params$GD2_aEvents_names, collapse = "|"),
      x = names(l_params)
    )
  ]
  GD2_aEvents_probs <- AE_data[paste0("prob_", l_params$GD2_aEvents_names)] |>
    unlist()
  GD2_aEvents_costs <- AE_data[paste0("cost_", l_params$GD2_aEvents_names)] |>
    unlist()

  # Re-scaling annual probability
  GD2_aEvents_rates <- - log(1 - GD2_aEvents_probs)
  reScaled_rates    <- GD2_aEvents_rates * l_params$cycle_length
  reScaled_probs    <-  1 - exp(- reScaled_rates)

  # Calculating AE event cost per model cycle
  AE_event_costs    <- sum(reScaled_probs * GD2_aEvents_costs)

  return(AE_event_costs)
}

#' Calculate Post Progression Survival (PPS) Costs
#'
#' @inheritParams calculate_treatment_costs
#'
#' @return A scalar representing the costs associated with PPS.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Define model parameters
#' params <- c(
#'   time_horizon = 10,
#'   cycle_length = 1/12,
#'   disc_rate_costs = 0.035,
#'   disc_rate_qalys = 0.015,
#'   NeuroblastomaPSM::l_psm_parameters
#' )
#'
#' # Calculate treatment costs
#' pps_costs <- NeuroblastomaPSM::calculate_pps_costs(
#'  l_params = params,
#'  Temo_cycle_days = 21,
#'  Iri_cycle_days = 21
#' )
#'
#' pps_costs
#' }
calculate_pps_costs <- function(
    l_params,
    Temo_cycle_days,
    Iri_cycle_days) {
  # Calculate days off treatments
  Temo_off_dose_days <- Temo_cycle_days - l_params$Temo_dose_days
  Iri_off_dose_days <- Iri_cycle_days - l_params$Iri_dose_days

  # Get model cycle time points
  time_points <- seq(
    from = 0,
    to = l_params$time_horizon,
    by = l_params$cycle_length
  )

  # Calculate body surface area
  surface_area <- (l_params$body_weight * 0.035) + 0.1

  # Dosage per child per cycle in mg
  v_Temo_dosage_cycle <- rep(# mg Temo/day x bsa x days
    x = (ceiling((100 * surface_area) / l_params$Temo_unit_mg) *
           l_params$Temo_unit_price),
    times = l_params$Temo_dose_days
  )
  v_Iri_dosage_cycle  <- rep(# mg  Iri/day x bsa x days
    x = (ceiling((50 * surface_area) / l_params$Iri_unit_mg) *
           l_params$Iri_unit_price),
    times = l_params$Iri_dose_days
  )
  v_Temo_cool_off      <- rep(
    x = 0,
    times = Temo_off_dose_days
  )
  v_Iri_cool_off       <- rep(
    x = 0,
    times = Iri_off_dose_days
  )
  v_Temo_costs  <- rep(
    x = c(
      v_Temo_dosage_cycle,
      v_Temo_cool_off
    ),
    length.out = 1 +              # start from day zero
      (l_params$time_horizon * 365) # continue cycles to death or time horizon
  )
  v_Iri_costs   <- rep(
    x = c(
      v_Iri_dosage_cycle,
      v_Iri_cool_off
    ),
    length.out = 1 +              # start from day zero
      (l_params$time_horizon * 365) # continue cycles to death or time horizon
  )
  m_annual_costs <- cbind(
    v_pps_costs = v_Temo_costs + v_Iri_costs,
    days_in_time_horizon = (0:(365 * l_params$time_horizon)) / 365
  )

  # Summing costs to the values specified by 'time_points'
  # Create intervals for days in every cycle based on 'points'
  intervals <- cut(
    x = m_annual_costs[, "days_in_time_horizon"],
    breaks = time_points,
    include.lowest = TRUE,
    right = FALSE
  )

  # Costs sumed over the time_points
  PPS_costs <- c(
    0,                                        # PPS costs at time 0 is assumed 0
    tapply(
      X = m_annual_costs[, "v_pps_costs"],
      INDEX = intervals,
      FUN = sum
    ) |>
      `rownames<-`(NULL)
  )

  return(PPS_costs)
}
