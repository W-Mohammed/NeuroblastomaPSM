# Introduction to NeuroblastomaPSM

<!-- badges: start -->

[![GitHub release](https://img.shields.io/badge/R-HEDS-green)](https://img.shields.io/badge/R-hello-green)
[![License: CC BY-NC 4.0](https://licensebuttons.net/l/by-nc/4.0/80x15.png)](https://creativecommons.org/licenses/by-nc/4.0/)
![GitHub last commit](https://img.shields.io/github/last-commit/W-Mohammed/NeuroblastomaPSM)
![GitHub top language](https://img.shields.io/github/languages/top/W-Mohammed/NeuroblastomaPSM)
![GitHub repo size](https://img.shields.io/github/repo-size/W-Mohammed/NeuroblastomaPSM)
![GitHub forks](https://img.shields.io/github/forks/W-Mohammed/NeuroblastomaPSM)
[![GitHub pages](https://img.shields.io/github/deployments/W-Mohammed/NeuroblastomaPSM/github-pages)](https://w-mohammed.github.io/NeuroblastomaPSM/)

<!-- badges: end -->

The **NeuroblastomaPSM** (*Neuroblastoma Partitioned Survival Model*) package documents the cost-effectiveness of Dinutuximab Beta (Qarziba®) for the treatment of high-risk neuroblastoma patients after stem cell transplantation.

## Installation

You can install the development version of NeuroblastomaPSM from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("W-Mohammed/NeuroblastomaPSM")
```

## Building vignettes without installation

You can build the package vignettes by cloning the repository, restoring the `renv` library and building documents with:

``` r
# install.packages("renv")
renv::restore()
# install.packages("devtools")
devtools::document()
```
