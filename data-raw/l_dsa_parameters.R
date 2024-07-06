## code to prepare `l_dsa_parameters` dataset goes here

# Extract the upper and lower bounds for the default DSA parameters
l_dsa_parameters <- lapply(
  X = NeuroblastomaPSM::l_sensitivity_parameters,
  FUN = function(param) {
    list(
      lower_ci = param$lower_ci,
      upper_ci = param$upper_ci
    )
  }
)

usethis::use_data(l_dsa_parameters, overwrite = TRUE)
