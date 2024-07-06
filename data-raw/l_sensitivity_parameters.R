## code to prepare `l_sensitivity_parameters` dataset goes here

# Define sensitivity parameters from the Chinese paper (Add reference)
l_sensitivity_parameters <- list(
  body_weight = list(
    "mean" = l_psm_parameters$body_weight,
    "lower_ci" = l_psm_parameters$body_weight*0.667,
    "upper_ci" = l_psm_parameters$body_weight*1.333,
    "distribution" = "normal"
  ),
  GD2_unit_price = list(
    "mean" = l_psm_parameters$GD2_unit_price,
    "lower_ci" = l_psm_parameters$GD2_unit_price*0.8,
    "upper_ci" = l_psm_parameters$GD2_unit_price*1.2,
    "distribution" = "gamma"
  ),
  TT_unit_price = list(
    "mean" = l_psm_parameters$TT_unit_price,
    "lower_ci" = l_psm_parameters$TT_unit_price*0.8,
    "upper_ci" = l_psm_parameters$TT_unit_price*1.2,
    "distribution" = "gamma"
  ),
  prob_anemia = list(
    "mean" = l_psm_parameters$prob_anemia,
    "lower_ci" = l_psm_parameters$prob_anemia*0.8,
    "upper_ci" = l_psm_parameters$prob_anemia*1.2,
    "distribution" = "beta"
  ),
  prob_leukopenia = list(
    "mean" = l_psm_parameters$prob_leukopenia,
    "lower_ci" = l_psm_parameters$prob_leukopenia*0.8,
    "upper_ci" = l_psm_parameters$prob_leukopenia*1.2,
    "distribution" = "beta"
  ),
  prob_neutropenia = list(
    "mean" = l_psm_parameters$prob_neutropenia,
    "lower_ci" = l_psm_parameters$prob_neutropenia*0.8,
    "upper_ci" = l_psm_parameters$prob_neutropenia*1.2,
    "distribution" = "beta"
  ),
  prob_thrombocytopenia = list(
    "mean" = l_psm_parameters$prob_thrombocytopenia,
    "lower_ci" = l_psm_parameters$prob_thrombocytopenia*0.8,
    "upper_ci" = l_psm_parameters$prob_thrombocytopenia*1.2,
    "distribution" = "beta"
  ),
  prob_fever = list(
    "mean" = l_psm_parameters$prob_fever,
    "lower_ci" = l_psm_parameters$prob_fever*0.8,
    "upper_ci" = l_psm_parameters$prob_fever*1.2,
    "distribution" = "beta"
  ),
  prob_urticaria = list(
    "mean" = l_psm_parameters$prob_urticaria,
    "lower_ci" = l_psm_parameters$prob_urticaria*0.8,
    "upper_ci" = l_psm_parameters$prob_urticaria*1.2,
    "distribution" = "beta"
  ),
  prob_immunotherapy_related_pain = list(
    "mean" = l_psm_parameters$prob_immunotherapy_related_pain,
    "lower_ci" = l_psm_parameters$prob_immunotherapy_related_pain*0.8,
    "upper_ci" = l_psm_parameters$prob_immunotherapy_related_pain*1.2,
    "distribution" = "beta"
  ),
  prob_diarrhea = list(
    "mean" = l_psm_parameters$prob_fever,
    "lower_ci" = l_psm_parameters$prob_fever*0.8,
    "upper_ci" = l_psm_parameters$prob_fever*1.2,
    "distribution" = "beta"
  ),
  prob_hypotension = list(
    "mean" = l_psm_parameters$prob_hypotension,
    "lower_ci" = l_psm_parameters$prob_hypotension*0.8,
    "upper_ci" = l_psm_parameters$prob_hypotension*1.2,
    "distribution" = "beta"
  ),
  prob_vomiting = list(
    "mean" = l_psm_parameters$prob_vomiting,
    "lower_ci" = l_psm_parameters$prob_vomiting*0.8,
    "upper_ci" = l_psm_parameters$prob_vomiting*1.2,
    "distribution" = "beta"
  ),
  prob_infection = list(
    "mean" = l_psm_parameters$prob_infection,
    "lower_ci" = l_psm_parameters$prob_infection*0.8,
    "upper_ci" = l_psm_parameters$prob_infection*1.2,
    "distribution" = "beta"
  ),
  prob_hypersensitivity_reaction = list(
    "mean" = l_psm_parameters$prob_hypersensitivity_reaction,
    "lower_ci" = l_psm_parameters$prob_hypersensitivity_reaction*0.8,
    "upper_ci" = l_psm_parameters$prob_hypersensitivity_reaction*1.2,
    "distribution" = "beta"
  ),
  prob_capillary_leak = list(
    "mean" = l_psm_parameters$prob_capillary_leak,
    "lower_ci" = l_psm_parameters$prob_capillary_leak*0.8,
    "upper_ci" = l_psm_parameters$prob_capillary_leak*1.2,
    "distribution" = "beta"
  ),
  cost_anemia = list(
    "mean" = l_psm_parameters$cost_anemia,
    "lower_ci" = l_psm_parameters$cost_anemia*0.8,
    "upper_ci" = l_psm_parameters$cost_anemia*1.2,
    "distribution" = "gamma"
  ),
  cost_leukopenia= list(
    "mean" = l_psm_parameters$cost_leukopenia,
    "lower_ci" = l_psm_parameters$cost_leukopenia*0.8,
    "upper_ci" = l_psm_parameters$cost_leukopenia*1.2,
    "distribution" = "gamma"
  ),
  cost_neutropenia = list(
    "mean" = l_psm_parameters$cost_neutropenia,
    "lower_ci" = l_psm_parameters$cost_neutropenia*0.8,
    "upper_ci" = l_psm_parameters$cost_neutropenia*1.2,
    "distribution" = "gamma"
  ),
  cost_thrombocytopenia = list(
    "mean" = l_psm_parameters$cost_thrombocytopenia,
    "lower_ci" = l_psm_parameters$cost_thrombocytopenia*0.8,
    "upper_ci" = l_psm_parameters$cost_thrombocytopenia*1.2,
    "distribution" = "gamma"
  ),
  cost_fever = list(
    "mean" = l_psm_parameters$cost_fever,
    "lower_ci" = l_psm_parameters$cost_fever*0.8,
    "upper_ci" = l_psm_parameters$cost_fever*1.2,
    "distribution" = "gamma"
  ),
  cost_urticaria = list(
    "mean" = l_psm_parameters$cost_urticaria,
    "lower_ci" = l_psm_parameters$cost_urticaria*0.8,
    "upper_ci" = l_psm_parameters$cost_urticaria*1.2,
    "distribution" = "gamma"
  ),
  cost_diarrhea = list(
    "mean" = l_psm_parameters$cost_diarrhea,
    "lower_ci" = l_psm_parameters$cost_diarrhea*0.8,
    "upper_ci" = l_psm_parameters$cost_diarrhea*1.2,
    "distribution" = "gamma"
  ),
  cost_immunotherapy_related_pain = list(
    "mean" = l_psm_parameters$cost_immunotherapy_related_pain,
    "lower_ci" = l_psm_parameters$cost_immunotherapy_related_pain*0.8,
    "upper_ci" = l_psm_parameters$cost_immunotherapy_related_pain*1.2,
    "distribution" = "gamma"
  ),
  cost_vomiting = list(
    "mean" = l_psm_parameters$cost_vomiting,
    "lower_ci" = l_psm_parameters$cost_vomiting*0.8,
    "upper_ci" = l_psm_parameters$cost_vomiting*1.2,
    "distribution" = "gamma"
  ),
  cost_hypotension  = list(
    "mean" = l_psm_parameters$cost_hypotension ,
    "lower_ci" = l_psm_parameters$cost_hypotension *0.8,
    "upper_ci" = l_psm_parameters$cost_hypotension *1.2,
    "distribution" = "gamma"
  ),
  cost_infection = list(
    "mean" = l_psm_parameters$cost_infection,
    "lower_ci" = l_psm_parameters$cost_infection*0.8,
    "upper_ci" = l_psm_parameters$cost_infection*1.2,
    "distribution" = "gamma"
  ),
  cost_hypersensitivity_reaction = list(
    "mean" = l_psm_parameters$cost_hypersensitivity_reaction,
    "lower_ci" = l_psm_parameters$cost_hypersensitivity_reaction*0.8,
    "upper_ci" = l_psm_parameters$cost_hypersensitivity_reaction*1.2,
    "distribution" = "gamma"
  ),
  cost_capillary_leak = list(
    "mean" = l_psm_parameters$cost_capillary_leak,
    "lower_ci" = l_psm_parameters$cost_capillary_leak*0.8,
    "upper_ci" = l_psm_parameters$cost_capillary_leak*1.2,
    "distribution" = "gamma"
  ),
  cost_pain = list(
    "mean" = l_psm_parameters$cost_pain,
    "lower_ci" = l_psm_parameters$cost_pain*0.8,
    "upper_ci" = l_psm_parameters$cost_pain*1.2,
    "distribution" = "gamma"
  ),
  cost_laboratory = list(
    "mean" = l_psm_parameters$cost_laboratory,
    "lower_ci" = l_psm_parameters$cost_laboratory*0.8,
    "upper_ci" = l_psm_parameters$cost_laboratory*1.2,
    "distribution" = "gamma"
  ),
  cost_hospitalisation = list(
    "mean" = l_psm_parameters$cost_hospitalisation,
    "lower_ci" = l_psm_parameters$cost_hospitalisation*0.8,
    "upper_ci" = l_psm_parameters$cost_hospitalisation*1.2,
    "distribution" = "gamma"
  ),
  Temo_unit_price = list(
    "mean" = l_psm_parameters$Temo_unit_price,
    "lower_ci" = l_psm_parameters$Temo_unit_price*0.95,
    "upper_ci" = l_psm_parameters$Temo_unit_price*1.05,
    "distribution" = "gamma"
  ),
  Iri_unit_price = list(
    "mean" = l_psm_parameters$Iri_unit_price,
    "lower_ci" = l_psm_parameters$Iri_unit_price*0.8,
    "upper_ci" = l_psm_parameters$Iri_unit_price*1.2,
    "distribution" = "gamma"
  ),
  u_EFS = list(
    "mean" = l_psm_parameters$u_EFS,
    "lower_ci" = l_psm_parameters$u_EFS*0.9,
    "upper_ci" = l_psm_parameters$u_EFS*1.1,
    "distribution" = "beta"
  ),
  u_PPS = list(
    "mean" = l_psm_parameters$u_PPS,
    "lower_ci" = l_psm_parameters$u_PPS*0.9,
    "upper_ci" = l_psm_parameters$u_PPS*1.1,
    "distribution" = "beta"
  )
)

# Estimate Standard Error:
l_sensitivity_parameters <- lapply(
  X = l_sensitivity_parameters,
  FUN = function(param) {
    NeuroblastomaPSM::estimate_se(
      mean = param$mean,
      lower_ci = param$lower_ci,
      upper_ci = param$upper_ci,
      distribution = param$distribution
    )
  }
)

usethis::use_data(l_sensitivity_parameters, overwrite = TRUE)
