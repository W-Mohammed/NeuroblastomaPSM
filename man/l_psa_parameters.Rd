% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{l_psa_parameters}
\alias{l_psa_parameters}
\title{Probabilistic Sensitivity Analysis (PSA) parameters}
\format{
A list with the following components:
\describe{
  \item{body_weight}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rnorm".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{mean}{Mean of the normal distribution.}
          \item{sd}{Standard deviation of the normal distribution.}
        }
      }
    }
  }
  \item{GD2_unit_price}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{TT_unit_price}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{prob_fever}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rbeta".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{alpha}{Alpha parameter of the beta distribution.}
          \item{beta}{Beta parameter of the beta distribution.}
        }
      }
    }
  }
  \item{prob_diarrhea}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rbeta".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{alpha}{Alpha parameter of the beta distribution.}
          \item{beta}{Beta parameter of the beta distribution.}
        }
      }
    }
  }
  \item{prob_vomiting}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rbeta".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{alpha}{Alpha parameter of the beta distribution.}
          \item{beta}{Beta parameter of the beta distribution.}
        }
      }
    }
  }
  \item{prob_infection}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rbeta".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{alpha}{Alpha parameter of the beta distribution.}
          \item{beta}{Beta parameter of the beta distribution.}
        }
      }
    }
  }
  \item{prob_hypersensitivity_reaction}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rbeta".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{alpha}{Alpha parameter of the beta distribution.}
          \item{beta}{Beta parameter of the beta distribution.}
        }
      }
    }
  }
  \item{prob_capillary_leak}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rbeta".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{alpha}{Alpha parameter of the beta distribution.}
          \item{beta}{Beta parameter of the beta distribution.}
        }
      }
    }
  }
  \item{cost_fever}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{cost_diarrhea}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{cost_vomiting}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{cost_infection}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{cost_hypersensitivity_reaction}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{cost_capillary_leak}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{cost_pain}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{cost_laboratory}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{cost_hospitalisation}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{Temo_unit_price}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{Iri_unit_price}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rgamma".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{shape}{Shape parameter of the gamma distribution.}
          \item{scale}{Scale parameter of the gamma distribution.}
        }
      }
    }
  }
  \item{u_EFS}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rbeta".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{alpha}{Alpha parameter of the beta distribution.}
          \item{beta}{Beta parameter of the beta distribution.}
        }
      }
    }
  }
  \item{u_PPS}{A list with the following elements:
    \describe{
      \item{dist_func}{Distribution function, here "rbeta".}
      \item{dist_params}{A list of parameters for the distribution:
        \describe{
          \item{alpha}{Alpha parameter of the beta distribution.}
          \item{beta}{Beta parameter of the beta distribution.}
        }
      }
    }
  }
}
}
\usage{
l_psa_parameters
}
\description{
This list contains the parameters used for a probabilistic sensitivity
analysis (PSA).
Each parameter includes the distribution function and the necessary
parameters for that distribution.
}
\keyword{datasets}
