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
  surveys <- samrat_read_surveymeta(surveymeta_file, pars)
  expect_true(inherits(surveys, "samrat_surveymeta"))

})

test_that("samrat_read_demography works", {

  param_file <- system.file("extdata/som_analysis_parameters.xlsx",
                            package = "samrat")
  pars <- samrat_read_params(param_file)

  demog_file <- system.file(
    "extdata/som_demog_data.xlsx", package = "samrat"
  )
  demog_pars <- samrat_read_demography(demog_file, pars)
  expect_equal(names(demog_pars), c("demog_pars", "pop_sources"))
  expect_true(inherits(demog_pars, "samrat_demog"))

  demog_pars$pop_sources <- list()
  expect_error(check_demog_file(demog_pars, pars), "pop_sources")

  demog_pars$demog_pars$assumed_cbr <- NULL
  expect_error(check_demog_file(demog_pars, pars), "variables")

})
