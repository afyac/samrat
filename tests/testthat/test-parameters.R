context("parameters")

test_that("samrat_read_params works", {

  param_file <- system.file(
    "extdata/som_analysis_parameters.xlsx", package = "samrat"
  )
  pars <- samrat_read_params(param_file)
  expect_equal(names(pars), c("gen_pars", "var_pars", "cf_pars"))

  pars$cf_pars$notes <- NULL
  expect_error(check_param_file(pars), "counterfactual")

  pars$var_pars$notes <- NULL
  expect_error(check_param_file(pars), "predictor")

  pars$gen_pars$adj_cdr_deff <- NULL
  expect_error(check_param_file(pars), "general")

})
