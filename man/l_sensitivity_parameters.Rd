% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{l_sensitivity_parameters}
\alias{l_sensitivity_parameters}
\title{Sensitivity Analysis parameters}
\format{
A list with the following components:
\describe{
  \item{body_weight}{A list with the following elements:
    \describe{
      \item{mean}{Mean body weight.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "normal".}
    }
  }
  \item{GD2_unit_price}{A list with the following elements:
    \describe{
      \item{mean}{Mean unit price for GD2.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{TT_unit_price}{A list with the following elements:
    \describe{
      \item{mean}{Mean unit price for TT.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{prob_anemia}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of anemia.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_leukopenia}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of leukopenia.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_neutropenia}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of neutropenia.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_thrombocytopenia}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of thrombocytopenia.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_fever}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of fever.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_urticaria}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of urticaria.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_immunotherapy_related_pain}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of immunotherapy related pain.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_diarrhea}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of diarrhea.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_hypotension}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of hypotension.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_vomiting}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of vomiting.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_infection}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of infection.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_hypersensitivity_reaction}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of hypersensitivity reaction.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{prob_capillary_leak}{A list with the following elements:
    \describe{
      \item{mean}{Mean probability of capillary leak.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{cost_anemia}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of anemia.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_leukopenia}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of leukopenia.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_neutropenia}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of neutropenia.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_thrombocytopenia}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of thrombocytopenia.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_fever}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of fever.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_urticaria}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of urticaria.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_immunotherapy_related_pain}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of immunotherapy related pain.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_diarrhea}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of diarrhea.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_hypotension}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of hypotension.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_vomiting}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of vomiting.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_infection}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of infection.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_hypersensitivity_reaction}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of hypersensitivity reaction.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_capillary_leak}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of capillary leak.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_pain}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of pain.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_laboratory}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of laboratory services.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{cost_hospitalisation}{A list with the following elements:
    \describe{
      \item{mean}{Mean cost of hospitalisation.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{Temo_unit_price}{A list with the following elements:
    \describe{
      \item{mean}{Mean unit price for Temo.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{Iri_unit_price}{A list with the following elements:
    \describe{
      \item{mean}{Mean unit price for Iri.}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "gamma".}
    }
  }
  \item{u_EFS}{A list with the following elements:
    \describe{
      \item{mean}{Mean utility for Event-Free Survival (EFS).}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
  \item{u_PPS}{A list with the following elements:
    \describe{
      \item{mean}{Mean utility for Post-Progression Survival (PPS).}
      \item{se}{Standard error of the mean.}
      \item{lower_ci}{Lower bound of the 95\% confidence interval.}
      \item{upper_ci}{Upper bound of the 95\% confidence interval.}
      \item{distribution}{Distribution type, here "beta".}
    }
  }
}
}
\usage{
l_sensitivity_parameters
}
\description{
This list contains the raw parameters used for the probabilistic sensitivity
analysis (PSA) and deterministic sensitivity analysis (DSA).
Each parameter includes its mean, standard error (se), lower and upper
bounds of the 95\% confidence interval, and the distribution type.
}
\keyword{datasets}
