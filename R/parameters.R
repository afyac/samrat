#' Read samrat parameters
#'
#' \code{samrat_read_params} Read samrat parameters
#'
#' @description Reads parameters from formatted `samrat` parameters file.
#'   TODO: Link here to correct format for a parameter file
#'
#' @param param_file Name of parameter file
#'
#' @return Returns a list with 4 elements:
#' \itemize{
#'       \item{"gen_pars"}{"General parameters `list`"}
#'       \item{"var_pars"}{"Predictor variables `data.frame`"}
#'       \item{"cf_pars"}{"Counterfactual variables `data.frame`}
#'       }
#'
#' @export
#' @examples {
#'
#' param_file <- system.file(
#' "extdata/som_analysis_parameters.xlsx", package="samrat"
#' )
#' pars <- samrat_read_params(param_file)
#' names(pars)
#'
#' }
samrat_read_params <- function(param_file) {

  # check file is suitable name and exists
  assert_string(param_file)
  assert_file_exists(param_file)

  #...................................
  ## General parameters
  gen_pars <- readxl::read_excel(param_file, sheet = "general_parameters")

  # turn this into a suitable list of our parameters
  gen_pars_list <- split(gen_pars$value, gen_pars$parameter)
  for (i in seq_along(gen_pars_list)) {
    if (suppressWarnings(!is.na(as.numeric(gen_pars_list[[i]])))) {
      gen_pars_list[[i]] <- as.numeric(gen_pars_list[[i]])
    }
  }

  #...................................
  ## Predictor-specific parameters
  var_pars <- readxl::read_excel(param_file, sheet = "predictor_parameters")
  var_pars <- as.data.frame(var_pars)

  #...................................
  ## Counterfactual parameters
  cf_pars <- readxl::read_excel(param_file, sheet = "counterfactual_parameters")
  cf_pars <- as.data.frame(cf_pars)

  #...................................
  ## TODO: Should this be read in here: Probably no
  # Sensitivity analysis parameters

  # Bring all the parameters into one list
  pars <- list(
    "gen_pars" = gen_pars_list,
    "var_pars" = var_pars,
    "cf_pars" = cf_pars
  )

  class(pars) <- c(class(pars), "samrat_params")
  return(pars)

}

#' Read samrat strata parameters
#'
#' \code{samrat_read_strata} Read samrat strata parameters
#'
#' @description Reads parameters from formatted `samrat` strata file.
#'   TODO: Link here to correct format for a parameter file
#'
#' @param strata_file Name of strata file
#' @param pars Parameter outputs from [samrat_read_params]
#' @return Returns a `data.frame` with 3 elements:
#' \itemize{
#'       \item{"admin0"}{"Admin Level 0"}
#'       \item{"admin1"}{"Admin Level 1"}
#'       \item{"strata"}{"Stratum level"}
#'       }
#'
#' @export
#' @examples {
#'
#' param_file <- system.file(
#' "extdata/som_analysis_parameters.xlsx", package="samrat"
#' )
#' pars <- samrat_read_params(param_file)
#'
#' strata_file <- system.file(
#' "extdata/som_analysis_strata.xlsx", package="samrat"
#' )
#' strata <- samrat_read_strata(strata_file, pars)
#' names(strata)
#'
#' }
samrat_read_strata <- function(strata_file, pars) {

  assert_string(strata_file)
  assert_file_exists(strata_file)
  assert_custom_class(pars, "samrat_params")

  #...................................
  ## Analysis strata
  strata <- readxl::read_excel(strata_file, sheet = "strata")
  strata <- as.data.frame(strata)

  # Rename country-specific geographic units
  colnames(strata)[colnames(strata) == pars$gen_pars$admin2_name] <- "stratum"
  colnames(strata)[colnames(strata) == pars$gen_pars$admin1_name] <- "admin1"

  # TODO: Suitable checks that strata data frame has all needed vars

  class(strata) <- c(class(strata), "samrat_strata")
  return(strata)

}

#' Read samrat survey metadata
#'
#' \code{samrat_read_surveymeta} Read samrat survey metadata
#'
#' @description Reads survey metadata from formatted `samrat` survey meta file.
#'   TODO: Link here to correct format for a parameter file
#'
#' @param surveymeta_file Name of survey meta file
#' @param pars Parameter outputs from [samrat_read_params]
#' @return Returns a data.frame with metadata on surveys available
#'
#' @export
#' @examples {
#'
#' param_file <- system.file(
#' "extdata/som_analysis_parameters.xlsx", package="samrat"
#' )
#' pars <- samrat_read_params(param_file)
#'
#' surveymeta_file <- system.file(
#' "extdata/som_survey_metadata.xlsx", package="samrat"
#' )
#' strata <- samrat_read_surveymeta(surveymeta_file, pars)
#'
#' }
samrat_read_surveymeta <- function(surveymeta_file, pars) {

  assert_string(surveymeta_file)
  assert_file_exists(surveymeta_file)
  assert_custom_class(pars, "samrat_params")

  #...................................
  ## Analysis strata
  smeta <- readxl::read_excel(surveymeta_file, sheet = "survey_metadata")
  smeta <- as.data.frame(smeta)

  # Rename country-specific geographic units
  colnames(smeta)[colnames(smeta) == pars$gen_pars$admin2_name] <- "stratum"
  colnames(smeta)[colnames(smeta) == pars$gen_pars$admin1_name] <- "admin1"

  # TODO: Other Country specifics, e.g. Unicef Nigeria LGAs

  # TODO: Suitable checks that survey meta data frame has all needed vars

  # Give class for later checks
  class(smeta) <- c(class(smeta), "samrat_surveymeta")
  return(smeta)

}

#' check_param_file
#'
#' \code{check_param_file} Checks param file is correctly structures
#'
#' @param pars Output of [samrat_read_params]
#'
#' @keywords internal

check_param_file <- function(pars) {

  # read in the example params from the package
  param_file <- system.file(
    "extdata/som_analysis_parameters.xlsx", package = "samrat"
  )
  pars_ex <- samrat_read_params(param_file)

  # simple check that the names are the same in gen_pars
  if (!all(names(pars_ex$gen_pars) %in% names(pars$gen_pars))) {
    stop("param_file general_parameters sheet is missing variables")
  }

  # simple check that the names are the same in var_pars
  if (!all(names(pars_ex$var_pars) %in% names(pars$var_pars))) {
    stop("param_file predictor_parameters sheet is missing variables")
  }

  # simple check that the names are the same in cf_pars
  if (!all(names(pars_ex$cf_pars) %in% names(pars$cf_pars))) {
    stop("param_file counterfactual_parameters sheet is missing variables")
  }

}
