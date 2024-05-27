## code to prepare `l_psm_parameters` dataset goes here

l_psm_parameters <- list(
  body_weight = 15,
  GD2_unit_mg = 20,
  GD2_dose_days = 10,
  GD2_unit_price = 8790.26,
  TT_unit_mg = 10,
  TT_dose_days = 14,
  TT_unit_price = 0.24,
  GD2_aEvents_names = c(
    "fever",
    "diarrhea",
    "vomiting",
    "infection",
    "hypersensitivity_reaction",
    "capillary_leak"
  ),
  prob_fever = 0.18,
  prob_diarrhea = 0.07,
  prob_vomiting = 0.06,
  prob_infection = 0.21,
  prob_hypersensitivity_reaction = 0.11,
  prob_capillary_leak = 0.06,
  cost_fever = 1.98,
  cost_diarrhea = 3.2,
  cost_vomiting = 8.45,
  cost_infection = 222.33,
  cost_hypersensitivity_reaction = 4.13,
  cost_capillary_leak = 307.37,
  cost_pain = 6.36,
  cost_laboratory = 141.45,
  cost_hospitalisation = 3763.62,
  Temo_unit_mg = 50,
  Temo_dose_days = 5,
  Temo_unit_price = 15.34,
  Iri_unit_mg = 40,
  Iri_dose_days = 5,
  Iri_unit_price =  4.4,
  u_EFS = 0.84,
  u_PPS = 0.56
)

usethis::use_data(l_psm_parameters, overwrite = TRUE)
