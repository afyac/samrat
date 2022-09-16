#' Read samrat parameters
#'
#' \code{samrat_read_params} Read samrat parameters
#'
#' @description Reads parameters from formatted `samrat` parameters file.
#'   TODO: Link here to correct format for a parameter file
#'
#' @param param_file Name of parameter file
#' @param check Boolean for whether to check the output. Default = `TRUE`
#'
#' @return Returns a list with 4 elements:
#' \itemize{
#'       \item{"pars_list"}{"General parameters `list`"}
#'       \item{"gen_pars"}{"General parameters `data.frame`"}
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
#' pars_list <- samrat_read_params(param_file)
#' names(pars_list)
#'
#' }
samrat_read_params <- function(param_file, check = TRUE) {

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
  pars_list <- list(
    "pars_list" = gen_pars_list,
    "gen_pars" = gen_pars,
    "var_pars" = var_pars,
    "cf_pars" = cf_pars
  )
  class(pars_list) <- c(class(pars_list), "samrat_params")

  # Run checks before returning
  if (check) check_param_file(pars_list)
  return(pars_list)

}

#' Read samrat strata parameters
#'
#' \code{samrat_read_strata} Read samrat strata parameters
#'
#' @description Reads parameters from formatted `samrat` strata file.
#'   TODO: Link here to correct format for a parameter file
#'
#' @param strata_file Name of strata file
#' @param pars_list Parameter outputs from [samrat_read_params]
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
#' pars_list <- samrat_read_params(param_file)
#'
#' strata_file <- system.file(
#' "extdata/som_analysis_strata.xlsx", package="samrat"
#' )
#' strata <- samrat_read_strata(strata_file, pars_list)
#' names(strata)
#'
#' }
samrat_read_strata <- function(strata_file, pars_list) {

  assert_string(strata_file)
  assert_file_exists(strata_file)
  assert_custom_class(pl, "samrat_params")
  pl <- pars_list

  #...................................
  ## Analysis strata
  strata <- readxl::read_excel(strata_file, sheet = "strata")
  strata <- as.data.frame(strata)

  # Rename country-specific geographic units
  colnames(strata)[colnames(strata) == pl$pars_list$admin2_name] <- "stratum"
  colnames(strata)[colnames(strata) == pl$pars_list$admin1_name] <- "admin1"

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
#' @param pars_list Parameter outputs from [samrat_read_params]
#' @return Returns a data.frame with metadata on surveys available
#'
#' @export
#' @examples {
#'
#' param_file <- system.file(
#' "extdata/som_analysis_parameters.xlsx", package="samrat"
#' )
#' pars_list <- samrat_read_params(param_file)
#'
#' surveymeta_file <- system.file(
#' "extdata/som_survey_metadata.xlsx", package="samrat"
#' )
#' strata <- samrat_read_surveymeta(surveymeta_file, pars_list)
#'
#' }
samrat_read_surveymeta <- function(surveymeta_file, pars_list) {

  assert_string(surveymeta_file)
  assert_file_exists(surveymeta_file)
  assert_custom_class(pars_list, "samrat_params")
  pl <- pars_list

  #...................................
  ## Analysis strata
  smeta <- readxl::read_excel(surveymeta_file, sheet = "survey_metadata")
  smeta <- as.data.frame(smeta)

  # Rename country-specific geographic units
  colnames(smeta)[colnames(smeta) == pl$pars_list$admin2_name] <- "stratum"
  colnames(smeta)[colnames(smeta) == pl$pars_list$admin1_name] <- "admin1"

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
#' @inheritParams samrat_read_params
#' @inheritParams samrat_read_strata
#' @param demog_file Name of demogrpahy file
#'
#' @return Returns a list with 5 elements:
#' \itemize{
#'       \item{"demog_pars_list"}{"Demography parameters `list`"}
#'       \item{"pop_sources_list"}{"Population source `list` of `data.frames`"}
#'       \item{"demog_pars"}{"Demography parameters `data.frame`"}
#'       \item{"pop_sources"}{"Population sources `data.frame`"}
#'       \item{"dictionary"}{"Demography Dictionary `data.frame`"}
#'       }
#'
#' @export
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' @examples {
#'
#' param_file <- system.file(
#' "extdata/som_analysis_parameters.xlsx", package="samrat"
#' )
#' pars_list <- samrat_read_params(param_file)
#'
#' demog_file <- system.file(
#' "extdata/som_demog_data.xlsx", package="samrat"
#' )
#' demography_list <- samrat_read_demography(demog_file, pars_list)
#' names(demography_list)
#'
#' }
samrat_read_demography <- function(demog_file, pars_list, check = TRUE) {

  assert_string(demog_file)
  assert_file_exists(demog_file)
  assert_custom_class(pars_list, "samrat_params")
  pl <- pars_list

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
  pop_sources <- pop_sources %>% filter(.data$used_in_analysis == "Y")

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
    quiet_message(paste("now importing population", x1))
    quiet_message(utils::str(x2))

    # only keep variables that will be used for analysis
    vars <- dictionary %>%
      filter(.data$worksheet == x1 & .data$used_in_analysis == "Y") %>%
      pull(.data$variable)
    x2 <- x2[, c(unlist(vars))]

    # rename country-specific geographic units
    colnames(x2) <- gsub(pl$pars_list$admin2_name, "stratum", colnames(x2))
    colnames(x2) <- gsub(pl$pars_list$admin1_name, "admin1", colnames(x2))

    # name the predictor as per the name of the worksheet
    pop_sources_list[[x1]] <- x2
  }


  # Bring all the parameters into one list
  demography_list <- list(
    "demog_pars_list" = demog_pars_list,
    "pop_sources_list" = pop_sources_list,
    "demog_pars" = demog_pars,
    "pop_sources" = pop_sources,
    "dictionary" = dictionary
  )
  class(demography_list) <- c(class(demography_list), "samrat_demography")

  # Run checks before returning
  if (check) check_demog_file(demography_list, pars_list)
  return(demography_list)

}

#' Read samrat predictor data
#'
#' \code{samrat_read_predictors} Read samrat predictors
#'
#' @description Reads predictors from formatted `samrat` predictors file.
#'   TODO: Link here to correct format for a predictors file
#'
#' @inheritParams samrat_read_params
#' @inheritParams samrat_read_strata
#' @param predictors_file Name of predictors file
#'
#' @return Returns a list with 3 elements:
#' \itemize{
#'       \item{"demog_pars"}{"Demography parameters `list`"}
#'       \item{"pop_sources"}{"Population source `list` of `data.frames`"}
#'       \item{"dictionary"}{"Demography Dictionary `data.frame`"}
#'       }
#'
#' @export
#' @importFrom dplyr filter pull
#' @importFrom rlang .data
#' @examples {
#'
#' param_file <- system.file(
#' "extdata/som_analysis_parameters.xlsx", package="samrat"
#' )
#' pars_list <- samrat_read_params(param_file)
#'
#' predictors_file <- system.file(
#' "extdata/som_predictor_data.xlsx", package="samrat"
#' )
#' predictors_list <- samrat_read_predictors(predictors_file, pars_list)
#' names(predictors_list)
#'
#' }
samrat_read_predictors <- function(predictors_file, pars_list, check = TRUE) {

  assert_string(predictors_file)
  assert_file_exists(predictors_file)
  assert_custom_class(pars_list, "samrat_params")
  pl <- pars_list

  #...................................
  ## Manual Imputations
  manual_imputations <- readxl::read_excel(
    predictors_file, sheet = "manual_imputations"
    )
  manual_imputations <- as.data.frame(manual_imputations)

  #...................................
  ## Predictor Data Sources
  predictors <- readxl::read_excel(predictors_file, sheet = "predictors_table")
  predictors <- as.data.frame(predictors)

  # exclude any datasets that are not going to be used for analysis
  predictors <- predictors %>% filter(.data$used_in_analysis == "Y")

  #...................................
  ## Dictionary variables needed to identify what variables are to be subset
  dictionary <- readxl::read_excel(predictors_file, sheet = "dictionary")
  dictionary <- as.data.frame(dictionary)


  # then read all of the population data sources and bring into list
  predictors_list <- vector("list", nrow(predictors))
  names(predictors_list) <- predictors$worksheet
  for (i in seq_len(nrow(predictors)))  {

    # what sheet are we reading in
    x1 <- paste(predictors$worksheet[i])

    # read pop sources sheet
    x2 <- readxl::read_excel(predictors_file, sheet = x1, na = "NA")
    x2 <- data.frame(x2)

    # check that import has been successful
    quiet_message("+++++++++++++++++++++++++++++++++++++")
    quiet_message(paste("now importing population", x1))
    quiet_message(utils::str(x2))

    # only keep variables that will be used for analysis
    vars <- dictionary %>%
      filter(.data$worksheet == x1 & .data$used_in_analysis == "Y") %>%
      pull(.data$variable)
    x2 <- x2[, c(unlist(vars))]

    # rename country-specific geographic units
    colnames(x2) <- gsub(pl$pars_list$admin2_name, "stratum", colnames(x2))
    colnames(x2) <- gsub(pl$pars_list$admin1_name, "admin1", colnames(x2))

    # name the predictor as per the name of the worksheet
    predictors_list[[x1]] <- x2
  }


  # Bring all the parameters into one list
  predictors_list <- list(
    "predictors_list" = predictors_list,
    "predictors" = predictors,
    "manual_imputations" = manual_imputations,
    "dictionary" = dictionary
  )
  class(predictors_list) <- c(class(predictors_list), "samrat_predictors")

  # Run checks before returning
  if (check) check_predictors_file(predictors_list, pars_list)
  return(predictors_list)

}

#' check_param_file
#'
#' \code{check_param_file} Checks param file is correctly structures
#'
#' @param pars_list Output of [samrat_read_params]
#'
#' @keywords internal

check_param_file <- function(pars_list) {

  # read in the example params from the package
  param_file <- system.file(
    "extdata/som_analysis_parameters.xlsx", package = "samrat"
  )
  ex <- samrat_read_params(param_file, check = FALSE)

  # simple check that the names are the same in gen_pars
  if (!all(names(ex$pars_list) %in% names(pars_list$pars_list))) {
    stop("param_file general_parameters list is missing variables")
  }

  # simple check that the names are the same in var_pars
  if (!all(names(ex$var_pars) %in% names(pars_list$var_pars))) {
    stop("param_file predictor_parameters sheet is missing variables")
  }

  # simple check that the names are the same in cf_pars
  if (!all(names(ex$cf_pars) %in% names(pars_list$cf_pars))) {
    stop("param_file counterfactual_parameters sheet is missing variables")
  }

}


#' check_demog_file
#'
#' \code{check_demog_file} Checks demog file is correctly structured
#'
#' @param demog_list Output of [samrat_read_demography]
#'
#' @keywords internal

check_demog_file <- function(demog_list, pars_list, check = FALSE) {

  # read in the example params from the package
  param_file <- system.file(
    "extdata/som_demog_data.xlsx", package = "samrat"
  )
  ex <- samrat_read_demography(param_file, pars_list, check)

  # simple check that the names are the same as with the example data
  if (!all(names(ex$demog_pars_list) %in% names(demog_list$demog_pars_list))) {
    stop("demog_file demog_pars sheet is missing variables")
  }

  # simple check that the class is the same for one of the list elements
  if (class(ex$pop_sources_list) != class(demog_list$pop_sources_list) ||
      length(demog_list$pop_sources_list) == 0) {
    stop("demog_file pop_sources sheets not correctly imported/formatted")
  }

}


#' check_predictors_file
#'
#' \code{check_predictors_file} Checks demog file is correctly structured
#'
#' @param predictors_list Output of [samrat_read_predictors]
#'
#' @keywords internal

check_predictors_file <- function(predictors_list, pars_list, check = FALSE) {

  # read in the example params from the package
  param_file <- system.file(
    "extdata/som_predictor_data.xlsx", package = "samrat"
  )
  ex <- samrat_read_predictors(param_file, pars_list, check)

  # simple check that the names are the same as with the example data
  if (!all(names(ex$predictors) %in% names(predictors_list$predictors))) {
    stop("predictors_file predictors_table sheet is missing variables")
  }

  # simple check that the class is the same for one of the list elements
  if (class(ex$predictors_list) != class(predictors_list$predictors_list) ||
      length(predictors_list$predictors_list) == 0) {
    stop("predictors_file predictors sheets not correctly imported/formatted")
  }

}
