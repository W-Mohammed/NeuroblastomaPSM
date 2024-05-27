## code to prepare `l_sensitivity_parameters` dataset goes here

# Define sensitivity parameters from the Chinese paper (Add reference)
l_sensitivity_parameters <- list(
  body_weight = list(
    "mean" = 15,
    "lower_ci" = 10,
    "upper_ci" = 20,
    "distribution" = "normal"
  ),
  GD2_unit_price = list(
    "mean" = 8790.26,
    "lower_ci" = 7032.21,
    "upper_ci" = 10548.31,
    "distribution" = "gamma"
  ),
  TT_unit_price = list(
    "mean" = 0.24,
    "lower_ci" = 0.19,
    "upper_ci" = 0.29,
    "distribution" = "gamma"
  ),
  prob_fever = list(
    "mean" = 0.18,
    "lower_ci" = 0.14,
    "upper_ci" = 0.22,
    "distribution" = "beta"
  ),
  prob_diarrhea = list(
    "mean" = 0.07,
    "lower_ci" = 0.056,
    "upper_ci" = 0.084,
    "distribution" = "beta"
  ),
  prob_vomiting = list(
    "mean" = 0.06,
    "lower_ci" = 0.048,
    "upper_ci" = 0.072,
    "distribution" = "beta"
  ),
  prob_infection = list(
    "mean" = 0.21,
    "lower_ci" = 0.16,
    "upper_ci" = 0.25,
    "distribution" = "beta"
  ),
  prob_hypersensitivity_reaction = list(
    "mean" = 0.11,
    "lower_ci" = 0.088,
    "upper_ci" = 0.13,
    "distribution" = "beta"
  ),
  prob_capillary_leak = list(
    "mean" = 0.06,
    "lower_ci" = 0.04,
    "upper_ci" = 0.08,
    "distribution" = "beta"
  ),
  cost_fever = list(
    "mean" = 1.98,
    "lower_ci" = 1.58,
    "upper_ci" = 2.38,
    "distribution" = "gamma"
  ),
  cost_diarrhea = list(
    "mean" = 3.2,
    "lower_ci" = 2.56,
    "upper_ci" = 3.84,
    "distribution" = "gamma"
  ),
  cost_vomiting = list(
    "mean" = 8.45,
    "lower_ci" = 6.76,
    "upper_ci" = 10.14,
    "distribution" = "gamma"
  ),
  cost_infection = list(
    "mean" = 222.33,
    "lower_ci" = 177.86,
    "upper_ci" = 266.80,
    "distribution" = "gamma"
  ),
  cost_hypersensitivity_reaction = list(
    "mean" = 4.13,
    "lower_ci" = 3.30,
    "upper_ci" = 4.96,
    "distribution" = "gamma"
  ),
  cost_capillary_leak = list(
    "mean" = 307.37,
    "lower_ci" = 245.90,
    "upper_ci" = 368.84,
    "distribution" = "gamma"
  ),
  cost_pain = list(
    "mean" = 6.36,
    "lower_ci" = 5.09,
    "upper_ci" = 7.63,
    "distribution" = "gamma"
  ),
  cost_laboratory = list(
    "mean" = 141.45,
    "lower_ci" = 113.16,
    "upper_ci" = 169.74,
    "distribution" = "gamma"
  ),
  cost_hospitalisation = list(
    "mean" = 3763.62,
    "lower_ci" = 3010.90,
    "upper_ci" = 4516.34,
    "distribution" = "gamma"
  ),
  Temo_unit_price = list(
    "mean" = 15.34,
    "lower_ci" = 12.27,
    "upper_ci" = 18.41,
    "distribution" = "gamma"
  ),
  Iri_unit_price = list(
    "mean" = 4.4,
    "lower_ci" = 3.52,
    "upper_ci" = 5.28,
    "distribution" = "gamma"
  ),
  u_EFS = list(
    "mean" = 0.84,
    "lower_ci" = 0.76,
    "upper_ci" = 0.92,
    "distribution" = "beta"
  ),
  u_PPS = list(
    "mean" = 0.56,
    "lower_ci" = 0.50,
    "upper_ci" = 0.62,
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
