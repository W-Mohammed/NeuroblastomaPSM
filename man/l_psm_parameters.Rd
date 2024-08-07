% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{l_psm_parameters}
\alias{l_psm_parameters}
\title{Partitioned Survival Model (PSM) Parameters}
\format{
A list with the following components:
\describe{
  \item{body_weight}{Body weight in kilograms, 15.}
  \item{GD2_unit_mg}{Unit dose of Dinutuximab β (GD2) in milligrams, 20.}
  \item{GD2_dose_days}{Number of days on Dinutuximab β per cycle, 5.}
  \item{GD2_unit_price}{Unit price of Dinutuximab β, 8790.26.}
  \item{TT_unit_mg}{Unit dose of Isotretinoin (TT) in milligrams, 10.}
  \item{TT_dose_days}{Number of days for Isotretinoin dosing, 14.}
  \item{TT_unit_price}{Unit price of Isotretinoin, 0.24.}
  \item{GD2_aEvents_names}{Names of adverse events for GD2:
    \describe{
      \item{fever}{Fever}
      \item{diarrhea}{Diarrhea}
      \item{vomiting}{Vomiting}
      \item{infection}{Infection}
      \item{hypersensitivity_reaction}{Hypersensitivity reaction}
      \item{capillary_leak}{Capillary leak}
    }
  }
  \item{prob_fever}{Probability of fever, 0.18.}
  \item{prob_diarrhea}{Probability of diarrhea, 0.07.}
  \item{prob_vomiting}{Probability of vomiting, 0.06.}
  \item{prob_infection}{Probability of infection, 0.21.}
  \item{prob_hypersensitivity_reaction}{Probability of hypersensitivity
  reaction, 0.11.}
  \item{prob_capillary_leak}{Probability of capillary leak, 0.06.}
  \item{cost_fever}{Cost of fever, 1.98.}
  \item{cost_diarrhea}{Cost of diarrhea, 3.2.}
  \item{cost_vomiting}{Cost of vomiting, 8.45.}
  \item{cost_infection}{Cost of infection, 222.33.}
  \item{cost_hypersensitivity_reaction}{Cost of hypersensitivity reaction, 4.13.}
  \item{cost_capillary_leak}{Cost of capillary leak, 307.37.}
  \item{cost_pain}{Cost of pain, 6.36.}
  \item{cost_laboratory}{Cost of laboratory services, 141.45.}
  \item{cost_hospitalisation}{Cost of hospitalisation, 3763.62.}
  \item{Temo_unit_mg}{Unit dose of Temozolomide (Temo) in milligrams, 50.}
  \item{Temo_dose_days}{Number of days on Temozolomide per cycle, 5.}
  \item{Temo_unit_price}{Unit price of Temozolomide, 15.34.}
  \item{Iri_unit_mg}{Unit dose of Irinotecan (Iri) in milligrams, 40.}
  \item{Iri_dose_days}{Number of days on Irinotecan per cycle, 5.}
  \item{Iri_unit_price}{Unit price of Irinotecan, 4.4.}
  \item{u_EFS}{Utility value for Event-Free Survival (EFS), 0.84.}
  \item{u_PPS}{Utility value for Post-Progression Survival (PPS), 0.56.}
}
}
\usage{
l_psm_parameters
}
\description{
This list contains the Partitioned Survival Model (PSM) parameters including
various cost, probability, and utility values used in the model.
}
\keyword{datasets}
