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
  gen_pars_list <- par_list_from_df(gen_pars)

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

  # Run checks before returning
  check_param_file(pars)
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

#' Read samrat demographic data
#'
#' \code{samrat_read_demography} Read samrat demography data
#'
#' @description Reads demography from formatted `samrat` demog_pars file.
#'   TODO: Link here to correct format for a parameter file
#'
#' @param demog_file Name of demogrpahy file
#' @inheritParams pars samrat_read_strata
#'
#' @return Returns a list with 2 elements:
#' \itemize{
#'       \item{"demog_pars"}{"Demography parameters `list`"}
#'       \item{"pop_sources"}{"Population source `list` of `data.frames`"}
#'       }
#'
#' @export
#' @examples {
#'
#' demog_file <- system.file(
#' "extdata/som_demog_data.xlsx", package="samrat"
#' )
#' demog_pars <- samrat_read_demography(param_file)
#' names(demog_pars)
#'
#' }
samrat_read_demography <- function(demog_file, pars) {

  assert_string(demog_file)
  assert_file_exists(demog_file)
  assert_custom_class(pars, "samrat_params")

  #...................................
  ## Demog pars
  demog_pars <- readxl::read_excel(demog_file, sheet = "demog_pars")

  # turn this into a suitable list of our parameters
  demog_pars_list <- par_list_from_df(demog_pars)

  #...................................
  ## Population Data Sources
  pop_sources <- readxl::read_excel(demog_file, sheet = "data_table")
  pop_sources <- as.data.frame(pop_sources)

  # exclude any datasets that are not going to be used for analysis
  pop_sources <- subset(pop_sources, used_in_analysis == "Y" )

  #...................................
  ## Dictionary variables needed to identify what variables are to be subset
  dictionary <- readxl::read_excel(demog_file, sheet = "dictionary")
  dictionary <- as.data.frame(dictionary)


  # then read all of the population data sources and bring into list
  pop_sources_list <- vector("list", nrow(pop_sources))
  names(pop_sources_list) <- pop_sources$worksheet
  for (i in seq_len(nrow(pop_sources)))  {

    # what sheet are we reading in
    x1 <- paste(pop_sources$worksheet[i])

    # read pop sources sheet
    x2 <- readxl::read_excel(demog_file, sheet = x1)
    x2 <- data.frame(x2)

    # check that import has been successful
    quiet_message("+++++++++++++++++++++++++++++++++++++")
    quiet_message(paste("now importing population", x1) )
    quiet_message(str(x2))

    # only keep variables that will be used for analysis
    vars <- subset(dictionary, worksheet == x1 & used_in_analysis == "Y")[, "variable"]
    x2 <- x2[, c(unlist(vars))]

    # rename country-specific geographic units
    colnames(x2) <- gsub(pars$gen_pars$admin2_name, "stratum", colnames(x2))
    colnames(x2) <- gsub(pars$gen_pars$admin1_name, "admin1", colnames(x2))

    # name the predictor as per the name of the worksheet
    pop_sources_list[[x1]] <- x2
  }


  # Bring all the parameters into one list
  demog_pars <- list(
    "demog_pars" = demog_pars_list,
    "pop_sources" = pop_sources_list
  )
  class(demog_pars) <- c(class(demog_pars), "samrat_demog")

  # Run checks before returning
  check_demog_file(demog_pars, pars)
  return(demog_pars)

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


#' check_demog_file
#'
#' \code{check_demog_file} Checks demog file is correctly structured
#'
#' @param demog_pars Output of [samrat_read_demography]
#'
#' @keywords internal

check_demog_file <- function(demog_pars, pars) {

  # read in the example params from the package
  param_file <- system.file(
    "extdata/som_demog_data.xlsx", package = "samrat"
  )
  pars_ex <- samrat_read_demography(param_file, pars)

  # simple check that the names are the same in gen_pars
  if (!all(names(pars_ex$demog_pars) %in% names(demog_pars$demog_pars))) {
    stop("demog_file demog_pars sheet is missing variables")
  }

  # simple check that the names are the same in var_pars
  if (class(pars_ex$pop_sources) != class(demog_pars$pop_sources) &&
      length(demog_pars$pop_sources == 0)) {
    stop("demog_file pop_sources sheets not correctly imported or formatted")
  }

}
