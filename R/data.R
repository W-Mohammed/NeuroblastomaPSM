#' Partitioned Survival Model (PSM) Parameters
#'
#' This list contains the Partitioned Survival Model (PSM) parameters including
#' various cost, probability, and utility values used in the model.
#'
#' @format A list with the following components:
#' \describe{
#'   \item{body_weight}{Body weight in kilograms, 15.}
#'   \item{GD2_unit_mg}{Unit dose of Dinutuximab β (GD2) in milligrams, 20.}
#'   \item{GD2_dose_days}{Number of days on Dinutuximab β per cycle, 10.}
#'   \item{GD2_unit_price}{Unit price of Dinutuximab β, 8790.26.}
#'   \item{TT_unit_mg}{Unit dose of Isotretinoin (TT) in milligrams, 10.}
#'   \item{TT_dose_days}{Number of days for Isotretinoin dosing, 14.}
#'   \item{TT_unit_price}{Unit price of Isotretinoin, 0.24.}
#'   \item{GD2_aEvents_names}{Names of adverse events for GD2:
#'     \describe{
#'       \item{fever}{Fever}
#'       \item{diarrhea}{Diarrhea}
#'       \item{vomiting}{Vomiting}
#'       \item{infection}{Infection}
#'       \item{hypersensitivity_reaction}{Hypersensitivity reaction}
#'       \item{capillary_leak}{Capillary leak}
#'     }
#'   }
#'   \item{prob_fever}{Probability of fever, 0.18.}
#'   \item{prob_diarrhea}{Probability of diarrhea, 0.07.}
#'   \item{prob_vomiting}{Probability of vomiting, 0.06.}
#'   \item{prob_infection}{Probability of infection, 0.21.}
#'   \item{prob_hypersensitivity_reaction}{Probability of hypersensitivity
#'   reaction, 0.11.}
#'   \item{prob_capillary_leak}{Probability of capillary leak, 0.06.}
#'   \item{cost_fever}{Cost of fever, 1.98.}
#'   \item{cost_diarrhea}{Cost of diarrhea, 3.2.}
#'   \item{cost_vomiting}{Cost of vomiting, 8.45.}
#'   \item{cost_infection}{Cost of infection, 222.33.}
#'   \item{cost_hypersensitivity_reaction}{Cost of hypersensitivity reaction, 4.13.}
#'   \item{cost_capillary_leak}{Cost of capillary leak, 307.37.}
#'   \item{cost_pain}{Cost of pain, 6.36.}
#'   \item{cost_laboratory}{Cost of laboratory services, 141.45.}
#'   \item{cost_hospitalisation}{Cost of hospitalisation, 3763.62.}
#'   \item{Temo_unit_mg}{Unit dose of Temozolomide (Temo) in milligrams, 50.}
#'   \item{Temo_dose_days}{Number of days on Temozolomide per cycle, 5.}
#'   \item{Temo_unit_price}{Unit price of Temozolomide, 15.34.}
#'   \item{Iri_unit_mg}{Unit dose of Irinotecan (Iri) in milligrams, 40.}
#'   \item{Iri_dose_days}{Number of days on Irinotecan per cycle, 5.}
#'   \item{Iri_unit_price}{Unit price of Irinotecan, 4.4.}
#'   \item{u_EFS}{Utility value for Event-Free Survival (EFS), 0.84.}
#'   \item{u_PPS}{Utility value for Post-Progression Survival (PPS), 0.56.}
#' }
"l_psm_parameters"

#' Sensitivity Analysis parameters
#'
#' This list contains the raw parameters used for the probabilistic sensitivity
#' analysis (PSA) and deterministic sensitivity analysis (DSA).
#' Each parameter includes its mean, standard error (se), lower and upper
#' bounds of the 95\% confidence interval, and the distribution type.
#'
#' @format A list with the following components:
#' \describe{
#'   \item{body_weight}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean body weight.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "normal".}
#'     }
#'   }
#'   \item{GD2_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean unit price for GD2.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{TT_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean unit price for TT.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{prob_fever}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of fever.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{prob_diarrhea}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of diarrhea.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{prob_vomiting}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of vomiting.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{prob_infection}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of infection.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{prob_hypersensitivity_reaction}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of hypersensitivity reaction.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{prob_capillary_leak}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of capillary leak.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{cost_fever}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of fever.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_diarrhea}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of diarrhea.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_vomiting}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of vomiting.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_infection}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of infection.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_hypersensitivity_reaction}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of hypersensitivity reaction.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_capillary_leak}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of capillary leak.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_pain}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of pain.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_laboratory}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of laboratory services.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_hospitalisation}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of hospitalisation.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{Temo_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean unit price for Temo.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{Iri_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean unit price for Iri.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{u_EFS}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean utility for Event-Free Survival (EFS).}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{u_PPS}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean utility for Post-Progression Survival (PPS).}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#' }
"l_sensitivity_parameters"

#' Probabilistic Sensitivity Analysis (PSA) parameters
#'
#' This list contains the parameters used for a probabilistic sensitivity
#' analysis (PSA).
#' Each parameter includes the distribution function and the necessary
#' parameters for that distribution.
#'
#' @format A list with the following components:
#' \describe{
#'   \item{body_weight}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rnorm".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{mean}{Mean of the normal distribution.}
#'           \item{sd}{Standard deviation of the normal distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{GD2_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{TT_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{prob_fever}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rbeta".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{alpha}{Alpha parameter of the beta distribution.}
#'           \item{beta}{Beta parameter of the beta distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{prob_diarrhea}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rbeta".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{alpha}{Alpha parameter of the beta distribution.}
#'           \item{beta}{Beta parameter of the beta distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{prob_vomiting}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rbeta".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{alpha}{Alpha parameter of the beta distribution.}
#'           \item{beta}{Beta parameter of the beta distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{prob_infection}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rbeta".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{alpha}{Alpha parameter of the beta distribution.}
#'           \item{beta}{Beta parameter of the beta distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{prob_hypersensitivity_reaction}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rbeta".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{alpha}{Alpha parameter of the beta distribution.}
#'           \item{beta}{Beta parameter of the beta distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{prob_capillary_leak}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rbeta".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{alpha}{Alpha parameter of the beta distribution.}
#'           \item{beta}{Beta parameter of the beta distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{cost_fever}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{cost_diarrhea}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{cost_vomiting}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{cost_infection}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{cost_hypersensitivity_reaction}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{cost_capillary_leak}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{cost_pain}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{cost_laboratory}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{cost_hospitalisation}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{Temo_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{Iri_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rgamma".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{shape}{Shape parameter of the gamma distribution.}
#'           \item{scale}{Scale parameter of the gamma distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{u_EFS}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rbeta".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{alpha}{Alpha parameter of the beta distribution.}
#'           \item{beta}{Beta parameter of the beta distribution.}
#'         }
#'       }
#'     }
#'   }
#'   \item{u_PPS}{A list with the following elements:
#'     \describe{
#'       \item{dist_func}{Distribution function, here "rbeta".}
#'       \item{dist_params}{A list of parameters for the distribution:
#'         \describe{
#'           \item{alpha}{Alpha parameter of the beta distribution.}
#'           \item{beta}{Beta parameter of the beta distribution.}
#'         }
#'       }
#'     }
#'   }
#' }
"l_psa_parameters"

#' Deterministic Sensitivity Analysis (DSA) parameters
#'
#' This list contains the parameters used for the deterministic sensitivity
#' analysis (DSA).
#' Each parameter includes its lower and upper bounds of the 95\% confidence
#' interval.
#'
#' @format A list with the following components:
#' \describe{
#'   \item{body_weight}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{GD2_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{TT_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{prob_fever}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{prob_diarrhea}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{prob_vomiting}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{prob_infection}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{prob_hypersensitivity_reaction}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{prob_capillary_leak}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_fever}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_diarrhea}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_vomiting}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_infection}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_hypersensitivity_reaction}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_capillary_leak}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_pain}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_laboratory}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_hospitalisation}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{Temo_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{Iri_unit_price}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{u_EFS}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{u_PPS}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#' }
"l_dsa_parameters"
