context("parameters")

test_that("samrat_read_params works", {

  param_file <- system.file(
    "extdata/som_analysis_parameters.xlsx", package = "samrat"
  )
  pars <- samrat_read_params(param_file)
  expect_equal(names(pars), c("gen_pars", "var_pars", "cf_pars"))

})
