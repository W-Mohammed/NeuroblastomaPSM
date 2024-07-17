testthat::test_that("markov trace estimated correctly", {
  # Load inputs data
  ## Survival curves
  df_survival_curves_long <- readRDS(
    file = testthat::test_path("testdata", "df_survival_curves_long.rds")
  )
  ## PSM parameters
  l_params <- readRDS(
    file = testthat::test_path("testdata", "l_params.rds")
  )
  # Load expected data
  expected <- readRDS(
    file = testthat::test_path("testdata", "df_markov_trace.rds")
  )
  # Test the function
  results <- NeuroblastomaPSM::calculate_markov_trace(
    df_survival_curves_long = df_survival_curves_long,
    l_params = l_params
  )
  # Unit tests
  testthat::expect_equal(expected, results)
  testthat::expect_true(
    all(rowSums(results[, c("EFS", "PPS", "D")]) == 1)
  )
})

testthat::test_that("markov trace with cure threshold estimated correctly", {
  # Load inputs data
  ## Survival curves
  df_survival_curves_long <- readRDS(
    file = testthat::test_path("testdata", "df_survival_curves_long2.rds")
  )
  ## PSM parameters
  l_params <- readRDS(
    file = testthat::test_path("testdata", "l_params2.rds")
  )
  # Load expected data
  expected <- readRDS(
    file = testthat::test_path("testdata", "df_markov_trace2.rds")
  )
  # Test the function
  results <- NeuroblastomaPSM::calculate_markov_trace(
    df_survival_curves_long = df_survival_curves_long,
    l_params = l_params
  )
  # Unit test
  testthat::expect_equal(expected, results)
  testthat::expect_true(
    all(rowSums(results[, c("EFS", "PPS", "D")]) == 1)
  )
})
