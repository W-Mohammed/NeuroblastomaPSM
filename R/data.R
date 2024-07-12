#' Jordan Life Table 2019
#'
#' This dataset provides mortality rates for different age groups, split by sex. The data includes mortality rates for both sexes combined, as well as separately for males and females.
#'
#' @format A data frame with 101 rows and 4 variables:
#' \describe{
#'   \item{Age Group}{Numeric vector representing the age groups from 0 to 100.}
#'   \item{Both sexes}{Numeric vector representing the mortality rates for both
#'   sexes combined.}
#'   \item{Male}{Numeric vector representing the mortality rates for males.}
#'   \item{Female}{Numeric vector representing the mortality rates for females.}
#' }
#' @source {Khader to add the reference!}
"df_lifeTable_Jordan"

#' Partitioned Survival Model (PSM) Parameters
#'
#' This list contains the Partitioned Survival Model (PSM) parameters including
#' various cost, probability, and utility values used in the model.
#'
#' @format A list with the following components:
#' \describe{
#'   \item{age}{Starting age of simulated cohort, 4.}
#'   \item{body_weight}{Body weight in kilograms, 15.}
#'   \item{smr_EFS}{Standardized Mortality Ratio (SMR) for the Event Free
#'   Survival compared to the general population all-cause mortality, 5.6.}
#'   \item{smr_PPS}{Standardized Mortality Ratio (SMR) for the Post Progression
#'   Survival compared to the general population all-cause mortality, 10.64.}
#'   \item{cure_threshold}{The time in years at which those in the EFS are
#'   considered cured, 10. Beyond this time, the mortality hazard rates for the
#'   EFS and PPS is set by the `smr_EFS` and `smr_PPS` parameters, respectively.}
#'   \item{GD2_unit_mg}{Unit dose of Dinutuximab β (GD2) in milligrams, 20.}
#'   \item{GD2_dose_days}{Number of days on Dinutuximab β per cycle, 5.}
#'   \item{GD2_unit_price}{Unit price of Dinutuximab β, 8790.26.}
#'   \item{TT_unit_mg}{Unit dose of Isotretinoin (TT) in milligrams, 20.}
#'   \item{TT_dose_days}{Number of days for Isotretinoin dosing, 14.}
#'   \item{TT_unit_price}{Unit price of Isotretinoin, 0.24.}
#'   \item{GD2_aEvents_names}{Names of adverse events for GD2:
#'     \describe{
#'       \item{anemia}{anemia}
#'       \item{leukopenia}{leukopenia}
#'       \item{neutropenia}{neutropenia}
#'       \item{thrombocytopenia}{thrombocytopenia}
#'       \item{fever}{Fever}
#'       \item{urticaria}{urticaria}
#'       \item{immunotherapy_related_pain}{immunotherapy related pain}
#'       \item{diarrhea}{Diarrhea}
#'       \item{hypotension}{hypotention}
#'       \item{vomiting}{Vomiting}
#'       \item{infection}{Infection}
#'       \item{hypersensitivity_reaction}{Hypersensitivity reaction}
#'       \item{capillary_leak}{Capillary leak}
#'     }
#'   }
#'   \item{prob_anemia}{Probability of anemia, 0.42}
#'   \item{prob_leukopenia}{Probability of leukopenia, 0.45}
#'   \item{prob_neutropenia}{Probability of neutropenia, 0.33}
#'   \item{prob_thrombocytopenia}{Probability of thrombocytopenia, 0.35}
#'   \item{prob_fever}{Probability of Fever, 0.14}
#'   \item{prob_urticaria}{Probability of urticaria, 0.05}
#'   \item{prob_immunotherapy_related_pain}{Probability of immunotherapy related pain, 0.16}
#'   \item{prob_diarrhea}{Probability of Diarrhea, 0.07}
#'   \item{prob_hypotension}{Probability of hypotention, 0.08}
#'   \item{prob_vomiting}{Probability of Vomiting, 0.06}
#'   \item{prob_infection}{Probability of Infection, 0.25}
#'   \item{prob_hypersensitivity_reaction}{Probability of Hypersensitivity reaction, 0.08}
#'   \item{prob_capillary_leak}{Probability of Capillary leak, 0.1}
#'   \item{cost_anemia}{Cost of anemia, 0.}
#'   \item{cost_leukopenia}{Cost of leukopenia, 0}
#'   \item{cost_neutropenia}{Cost of neutropenia, 0}
#'   \item{cost_thrombocytopenia}{Cost of thrombocytopenia, 0}
#'   \item{cost_fever}{Cost of Fever, 1.98}
#'   \item{cost_urticaria}{Cost of urticaria, 0}
#'   \item{cost_immunotherapy_related_pain}{Cost of immunotherapy related pain, 0}
#'   \item{cost_diarrhea}{Cost of Diarrhea, 3.2}
#'   \item{cost_hypotension}{Cost of hypotention, 0}
#'   \item{cost_vomiting}{Cost of Vomiting, 8.45}
#'   \item{cost_infection}{Cost of Infection, 222.33}
#'   \item{cost_hypersensitivity_reaction}{Cost of Hypersensitivity reaction, 4.13}
#'   \item{cost_capillary_leak}{Cost of Capillary leak, 306.37}
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
#'   \item{df_life_table}{dataframe representing the Life Table for Jordan:
#'     \describe{
#'       \item{age}{Numeric vector representing the age groups from 0 to 100.}
#'       \item{prob}{Numeric vector representing the mortality rates.}
#'     }
#'   }
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
#'   \item{prob_anemia}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of anemia.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{prob_leukopenia}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of leukopenia.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{prob_neutropenia}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of neutropenia.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{prob_thrombocytopenia}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of thrombocytopenia.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
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
#'   \item{prob_urticaria}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of urticaria.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "beta".}
#'     }
#'   }
#'   \item{prob_immunotherapy_related_pain}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of immunotherapy related pain.}
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
#'   \item{prob_hypotension}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean probability of hypotension.}
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
#'   \item{cost_anemia}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of anemia.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_leukopenia}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of leukopenia.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_neutropenia}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of neutropenia.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_thrombocytopenia}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of thrombocytopenia.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
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
#'   \item{cost_urticaria}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of urticaria.}
#'       \item{se}{Standard error of the mean.}
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'       \item{distribution}{Distribution type, here "gamma".}
#'     }
#'   }
#'   \item{cost_immunotherapy_related_pain}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of immunotherapy related pain.}
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
#'   \item{cost_hypotension}{A list with the following elements:
#'     \describe{
#'       \item{mean}{Mean cost of hypotension.}
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
#'   \item{prob_anemia}{A list with the following elements:
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
#'   \item{prob_leukopenia}{A list with the following elements:
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
#'   \item{prob_neutropenia}{A list with the following elements:
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
#'   \item{prob_thrombocytopenia}{A list with the following elements:
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
#'   \item{prob_urticaria}{A list with the following elements:
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
#'   \item{prob_immunotherapy_related_pain}{A list with the following elements:
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
#'   \item{prob_hypotension}{A list with the following elements:
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
#'   \item{cost_anemia}{A list with the following elements:
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
#'   \item{cost_leukopenia}{A list with the following elements:
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
#'   \item{cost_neutropenia}{A list with the following elements:
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
#'   \item{cost_thrombocytopenia}{A list with the following elements:
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
#'   \item{cost_urticaria}{A list with the following elements:
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
#'   \item{cost_immunotherapy_related_pain}{A list with the following elements:
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
#'   \item{cost_hypotension}{A list with the following elements:
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
#'   \item{prob_anemia}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{prob_leukopenia}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{prob_neutropenia}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{prob_thrombocytopenia}{A list with the following elements:
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
#'   \item{prob_urticaria}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{prob_immunotherapy_related_pain}{A list with the following elements:
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
#'   \item{prob_hypotension}{A list with the following elements:
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
#'   \item{cost_anemia}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_leukopenia}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_neutropenia}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_thrombocytopenia}{A list with the following elements:
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
#'   \item{cost_urticaria}{A list with the following elements:
#'     \describe{
#'       \item{lower_ci}{Lower bound of the 95\% confidence interval.}
#'       \item{upper_ci}{Upper bound of the 95\% confidence interval.}
#'     }
#'   }
#'   \item{cost_immunotherapy_related_pain}{A list with the following elements:
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
#'   \item{cost_hypotension}{A list with the following elements:
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
