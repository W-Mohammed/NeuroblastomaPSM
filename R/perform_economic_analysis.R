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
#'   params = params
#' )
#'
#' # Generate Markov trace
#' df_markov_trace <- NeuroblastomaPSM::calculate_markov_trace(
#'   df_survival_curves_long = df_survival_curves_long
#' )
#'
#' # Perform Economic Analysis
#' v_psm_results <- NeuroblastomaPSM::perform_economic_analysis(
#'   df_markov_trace = df_markov_trace,
#'   params = params
#' )
#'
#' v_psm_results
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
    (v_Util * params$cycle_length)
  v_qalys_Dinutuximab_β <- m_TR_Dinutuximab_β %*%
    (v_Util * params$cycle_length)

  # Get model cycles time points
  time_points <- seq(
    from = 0,
    to = params$time_horizon,
    by = params$cycle_length
  )

  # Calculate costs and QALYs discount weights
  v_dw_c <- 1 / (1 + params$disc_rate_costs) ^ time_points
  v_dw_e <- 1 / (1 + params$disc_rate_qalys) ^ time_points

  # Prepare un-discounted results
  v_costs_results <- cbind(rowSums(m_costs_Isotretinoin),
                           rowSums(m_costs_Dinutuximab_β)) |>
    `colnames<-`(c("costs_Isotretinoin", "costs_Dinutuximab_β"))
  v_qalys_results <- cbind(v_qalys_Isotretinoin, v_qalys_Dinutuximab_β) |>
    `colnames<-`(c("qalys_Isotretinoin", "qalys_Dinutuximab_β"))

  # Prepare discounted results
  v_Dcosts_results <- v_dw_c %*% v_costs_results |>
    `colnames<-`(c("Dcosts_Isotretinoin", "Dcosts_Dinutuximab_β")) |>
    _[1, ]
  v_Dqalys_results <- v_dw_e %*% v_qalys_results |>
    `colnames<-`(c("Dqalys_Isotretinoin", "Dqalys_Dinutuximab_β")) |>
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
#' l_treatment_costs <- calculate_treatment_costs(params = params)
#'
#' View(l_treatment_costs)
#' }
calculate_treatment_costs <- function(
    params,
    GD2_cycle_days = 35,
    TT_cycle_days = 30,
    Temo_cycle_days = 21,
    Iri_cycle_days = 21) {
  # Estimate EFS costs
  l_efs_costs <- calculate_efs_costs(
    params = params,
    GD2_cycle_days = GD2_cycle_days,
    TT_cycle_days = TT_cycle_days
  )

  # Estimate PPS costs
  v_pps_costs <- calculate_pps_costs(
    params = params,
    Temo_cycle_days = Temo_cycle_days,
    Iri_cycle_days = Iri_cycle_days
  )

  # Get intervention groups costs together
  m_C_Dinutuximab_β <- cbind(
    EFS = l_efs_costs$GD2_EFS_costs,
    PPS = v_pps_costs,
    D = 0
  )
  m_C_Isotretinoin <- cbind(
    EFS = l_efs_costs$TT_EFS_costs,
    PPS = v_pps_costs,
    D = 0
  )

  return(
    list(
      m_C_Dinutuximab_β = m_C_Dinutuximab_β,
      m_C_Isotretinoin = m_C_Isotretinoin
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
#' l_GD2_efs_costs <- calculate_efs_costs(
#'  params = params,
#'  GD2_cycle_days = 35,
#'  TT_cycle_days = 30
#' )
#'
#' l_GD2_efs_costs
#' }
calculate_efs_costs <- function(
    params,
    GD2_cycle_days,
    TT_cycle_days) {
  # Calculate days off treatments
  GD2_off_dose_days <- GD2_cycle_days - (params$GD2_dose_days +
                                           params$TT_dose_days)
  TT_off_dose_days <- TT_cycle_days - params$TT_dose_days
  # Estimate GD2 AE costs per cycle length
  GD2_AE_costs_cycle <- calculate_ae_costs(params = params)
  GD2_AE_costs_day <- GD2_AE_costs_cycle / (params$cycle_length * 365)

  # Get model cycle time points
  time_points <- seq(
    from = 0,
    to = params$time_horizon,
    by = params$cycle_length
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
    rep(
      x = c(
        v_GD2_dosage_cycle + GD2_AE_costs_day, # adding possible AE costs
        v_TT_dosage_cycle  + GD2_AE_costs_day, # adding possible AE costs
        v_GD2_cool_off     + GD2_AE_costs_day  # adding possible AE costs
      ),
      times = 5
    ),
    v_TT_dosage_cycle      + GD2_AE_costs_day, # adding possible AE costs
    v_GD2_cool_off         + GD2_AE_costs_day  # adding possible AE costs
  )
  v_annual_TT_costs   <- c(
    rep(
      x = c(
        v_TT_dosage_cycle,
        v_TT_cool_off
      ),
      times = 5
    ),
    v_TT_dosage_cycle
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
#' GD2_ae_costs <- calculate_ae_costs(params = params)
#'
#' GD2_ae_costs
#' }
calculate_ae_costs <- function(params) {
  # Get model cycle time points
  time_points <- seq(
    from = 0,
    to = params$time_horizon,
    by = params$cycle_length
  )

  # Combine GD2 AE probs and costs
  AE_data <- params[
    grepl(
      pattern = paste0(params$GD2_aEvents_names, collapse = "|"),
      x = names(params)
    )
  ]
  GD2_aEvents_probs <- AE_data[paste0("prob_", params$GD2_aEvents_names)] |>
    unlist()
  GD2_aEvents_costs <- AE_data[paste0("cost_", params$GD2_aEvents_names)] |>
    unlist()

  # Re-scaling annual probability
  GD2_aEvents_rates <- - log(1 - GD2_aEvents_probs)
  reScaled_rates    <- GD2_aEvents_rates * params$cycle_length
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
#' pps_costs <- calculate_pps_costs(
#'  params = params,
#'  Temo_cycle_days = 21,
#'  Iri_cycle_days = 21
#' )
#'
#' pps_costs
#' }
calculate_pps_costs <- function(
    params,
    Temo_cycle_days,
    Iri_cycle_days) {
  # Calculate days off treatments
  Temo_off_dose_days <- Temo_cycle_days - params$Temo_dose_days
  Iri_off_dose_days <- Iri_cycle_days - params$Iri_dose_days

  # Get model cycle time points
  time_points <- seq(
    from = 0,
    to = params$time_horizon,
    by = params$cycle_length
  )

  # Calculate body surface area
  surface_area <- (params$body_weight * 0.035) + 0.1

  # Dosage per child per cycle in mg
  v_Temo_dosage_cycle <- rep(# mg Temo/day x bsa x days
    x = (ceiling((100 * surface_area) / params$Temo_unit_mg) *
           params$Temo_unit_price),
    times = params$Temo_dose_days
  )
  v_Iri_dosage_cycle  <- rep(# mg  Iri/day x bsa x days
    x = (ceiling((50 * surface_area) / params$Iri_unit_mg) *
           params$Iri_unit_price),
    times = params$Iri_dose_days
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
      (params$time_horizon * 365) # continue cycles to death or time horizon
  )
  v_Iri_costs   <- rep(
    x = c(
      v_Iri_dosage_cycle,
      v_Iri_cool_off
    ),
    length.out = 1 +              # start from day zero
      (params$time_horizon * 365) # continue cycles to death or time horizon
  )
  m_annual_costs <- cbind(
    v_pps_costs = v_Temo_costs + v_Iri_costs,
    days_in_time_horizon = (0:(365 * params$time_horizon)) / 365
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
