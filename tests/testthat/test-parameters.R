context("parameters")

test_that("samrat_read_params works", {

  param_file <- system.file("extdata/som_analysis_parameters.xlsx",
                            package = "samrat")
  pars <- samrat_read_params(param_file)
  expect_equal(names(pars), c("gen_pars", "var_pars", "cf_pars"))
  expect_true(inherits(pars, "samrat_params"))

  pars$cf_pars$notes <- NULL
  expect_error(check_param_file(pars), "counterfactual")

  pars$var_pars$notes <- NULL
  expect_error(check_param_file(pars), "predictor")

  pars$gen_pars$adj_cdr_deff <- NULL
  expect_error(check_param_file(pars), "general")

})

test_that("samrat_read_strata works", {

  param_file <- system.file("extdata/som_analysis_parameters.xlsx",
                            package = "samrat")
  pars <- samrat_read_params(param_file)

  strata_file <- system.file("extdata/som_analysis_strata.xlsx", package =
                               "samrat")
  strata <- samrat_read_strata(strata_file, pars)
  expect_equal(names(strata), c("admin0", "admin1", "stratum"))
  expect_true(inherits(strata, "samrat_strata"))

})

test_that("samrat_read_surveymeta works", {

  param_file <- system.file("extdata/som_analysis_parameters.xlsx",
                            package = "samrat")
  pars <- samrat_read_params(param_file)

  surveymeta_file <- system.file(
    "extdata/som_survey_metadata.xlsx", package = "samrat"
  )
  smeta <- samrat_read_surveymeta(surveymeta_file, pars)
  expect_true(inherits(smeta, "samrat_surveymeta"))

})
