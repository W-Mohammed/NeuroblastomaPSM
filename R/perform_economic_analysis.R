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
#'   \item \code{cycle_length}: The length of a model cycle measured in years.
#'   \item \code{body_weight}: The body weight of the patient in kilograms.
#'   \item \code{GD2_unit_mg}: The dosage unit for GD2 in milligrams. Default is
#'   `20` mg.
#'   \item \code{GD2_unit_price}: The price per unit dosage of GD2.
#'   \item \code{GD2_dose_days}: The number of days GD2 is administered in a
#'   cycle.
#'   \item \code{TT_unit_mg}: The dosage unit for TT in milligrams. Default is
#'   `10` mg.
#'   \item \code{TT_unit_price}: The price per unit dosage of TT.
#'   \item \code{TT_dose_days}: The number of days TT is administered in a
#'   cycle.
#'   \item \code{GD2_aEvents_names}: A vector containing the names of the
#'   adverse events associated with GD2.
#'   \item \code{GD2_aEvents_probs}: A named vector containing the annual
#'   probability of each GD2 adverse event.
#'   \item \code{GD2_aEvents_costs}: A named vector containing the costs
#'   associated with each GD2 adverse event.
#'   \item \code{Temo_unit_mg}: The dosage unit for Temozolomide in milligrams.
#'   Default is `50` mg.
#'   \item \code{Temo_unit_price}: The price per unit dosage of Temozolomide.
#'   \item \code{Temo_dose_days}: The number of days Temozolomide is
#'   administered in a cycle. Default is `5` days.
#'   \item \code{Iri_unit_mg}: The dosage unit for Irinotecan in milligrams.
#'   Default is `40` mg.
#'   \item \code{Iri_unit_price}: The price per unit dosage of Irinotecan.
#'   \item \code{Iri_dose_days}: The number of days Irinotecan is administered in
#'   a cycle. Default is `5` days.
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
#' @return A matrix of treatment costs for the first year, with columns for GD2
#' and TT costs over the specified time points.
#' @export
#' @examples
#' \dontrun{
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
#' # Calculate treatment costs
#' l_treatment_costs <- calculate_treatment_costs(params = params)
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
#' @return A list containing `Dinutuximab β` (GD2) and `Isotretinoin` (TT) costs
#' over the model time-horizon.
#' @export
#' @examples
#' \dontrun{
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
#' # Calculate AE treatment costs
#' l_GD2_efs_costs <- calculate_efs_costs(
#'  params = params,
#'  GD2_cycle_days = 35,
#'  TT_cycle_days = 30
#' )
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
#' @return A scalar representing the costs of related to `Dinutuximab β` (GD2)
#' adverse events.
#' @export
#' @examples
#' \dontrun{
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
#' # Calculate AE costs
#' GD2_ae_costs <- calculate_ae_costs(params = params)
#' GD2_ae_costs
#' }
calculate_ae_costs <- function(params) {
  # Get model cycle time points
  time_points <- seq(
    from = 0,
    to = params$time_horizon,
    by = params$cycle_length
  )

  # Re-scaling annual probability
  GD2_aEvents_rates <- - log(1 - params$GD2_aEvents_probs)
  reScaled_rates    <- GD2_aEvents_rates * params$cycle_length
  reScaled_probs    <-  1 - exp(- reScaled_rates)

  # Calculating AE event cost per model cycle
  AE_event_costs    <- sum(reScaled_probs * params$GD2_aEvents_costs)

  return(AE_event_costs)
}

#' Calculate Post Progression Survival (PPS) Costs
#'
#' @inheritParams calculate_treatment_costs
#' @return A scalar representing the costs associated with PPS.
#' @export
#' @examples
#' \dontrun{
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
#' # Calculate treatment costs
#' pps_costs <- calculate_pps_costs(
#'  params = params,
#'  Temo_cycle_days = 21,
#'  Iri_cycle_days = 21
#' )
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
