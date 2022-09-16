context("parameters")

test_that("samrat_read_params works", {

  param_file <- system.file("extdata/som_analysis_parameters.xlsx",
                            package = "samrat")
  pars_list <- samrat_read_params(param_file)
  expect_equal(
    names(pars_list),
    c("pars_list", "gen_pars", "var_pars", "cf_pars")
  )
  expect_true(inherits(pars_list, "samrat_params"))

  pars_list$cf_pars$notes <- NULL
  expect_error(check_param_file(pars_list), "counterfactual")

  pars_list$var_pars$notes <- NULL
  expect_error(check_param_file(pars_list), "predictor")

  pars_list$pars_list$adj_cdr_deff <- NULL
  expect_error(check_param_file(pars_list), "general")

})

test_that("samrat_read_strata works", {

  param_file <- system.file("extdata/som_analysis_parameters.xlsx",
                            package = "samrat")
  pars_list <- samrat_read_params(param_file)

  strata_file <- system.file("extdata/som_analysis_strata.xlsx", package =
                               "samrat")
  strata <- samrat_read_strata(strata_file, pars_list)
  expect_equal(names(strata), c("admin0", "admin1", "stratum"))
  expect_true(inherits(strata, "samrat_strata"))

})

test_that("samrat_read_surveymeta works", {

  param_file <- system.file("extdata/som_analysis_parameters.xlsx",
                            package = "samrat")
  pars_list <- samrat_read_params(param_file)

  surveymeta_file <- system.file(
    "extdata/som_survey_metadata.xlsx", package = "samrat"
  )
  surveys <- samrat_read_surveymeta(surveymeta_file, pars_list)
  expect_true(inherits(surveys, "samrat_surveymeta"))

})

test_that("samrat_read_demography works", {

  param_file <- system.file("extdata/som_analysis_parameters.xlsx",
                            package = "samrat")
  pars_list <- samrat_read_params(param_file)

  demog_file <- system.file(
    "extdata/som_demog_data.xlsx", package = "samrat"
  )
  demography_list <- samrat_read_demography(demog_file, pars_list)
  expect_equal(names(demography_list),
               c("demog_pars_list", "pop_sources_list",
                 "demog_pars", "pop_sources", "dictionary"))
  expect_true(inherits(demography_list, "samrat_demography"))

  demography_list$pop_sources_list <- list()
  expect_error(check_demog_file(demography_list, pars_list), "pop_sources")

  demography_list$demog_pars_list$assumed_cbr <- NULL
  expect_error(check_demog_file(demography_list, pars_list), "variables")

})


test_that("samrat_read_predictors works", {

  param_file <- system.file("extdata/som_analysis_parameters.xlsx",
                            package = "samrat")
  pars_list <- samrat_read_params(param_file)

  predictors_file <- system.file(
    "extdata/som_predictor_data.xlsx", package = "samrat"
  )

  pred_list <- samrat_read_predictors(predictors_file, pars_list)

  expect_equal(
    names(pred_list),
    c("predictors_list", "predictors", "manual_imputations", "dictionary")
  )
  expect_true(inherits(pred_list, "samrat_predictors"))

  pred_list$predictors_list <- list()
  expect_error(check_predictors_file(pred_list, pars_list), "predictors_file")

  pred_list$predictors$used_in_analysis <- NULL
  expect_error(check_demog_file(pred_list, pars_list), "variables")

})
