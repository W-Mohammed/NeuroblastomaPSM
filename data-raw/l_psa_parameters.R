## code to prepare `l_psa_parameters` dataset goes here

# Get the sampling parameters for the default PSA parameters
l_psa_parameters <- lapply(
  X = NeuroblastomaPSM::l_sensitivity_parameters,
  FUN = function(param) {
    NeuroblastomaPSM::get_sampling_params(
      mean = param$mean,
      se = param$se,
      distribution = param$distribution
    )
  }
)

usethis::use_data(l_psa_parameters, overwrite = TRUE)
