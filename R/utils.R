`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


#------------------------------------------------
#' read_samrat_file
#'
#' \code{read_samrat_file} read samrat package file
#'
#' @description Load a file from within the inst/extdata folder of the
#'   [samrat] package. File extension must be one of .csv, .rds, or .xlsx
#'
#' @param name Name of a file within the inst/extdata folder.
#' @param sheet Name of sheet in xlsx file. Default = `NULL`
#'
#' @importFrom utils read.csv
#' @importFrom readxl read_xlsx
#' @keywords internal

read_samrat_file <- function(name, sheet = NULL) {

  # check that valid file extension
  ext <- strsplit(name, "\\.")[[1]]
  ext <- ext[length(ext)]
  if (is.element(ext, c("csv", "rds", "xlsx")) == FALSE) {
    stop("file extension not valid")
  }

  # get full file path
  path <- system.file("extdata/", name, package = "samrat", mustWork = TRUE)

  # read in file
  if (ext == "rds") {
    ret <- readRDS(path)
  } else if (ext == "csv") {
    ret <- utils::read.csv(file = path, header = TRUE, sep = ",")
  } else {
    ret <- readxl::read_xlsx(path, sheet = sheet)
  }

  return(ret)

}


#------------------------------------------------
#' par_list_from_df
#'
#' \code{par_list_from_df} convert data.frame into formatted list
#'
#' @description Convert data frame with columns value and parameter into
#'   named list with numeric values formatted
#'
#' @param pars \code{data.frame} of parameters and values
#' @param value String for value column. Default = `"value"`
#' @param parameter String for value column. Default = `"parameter"`
#'
#' @keywords internal

par_list_from_df <- function(pars, value = "value", parameter = "parameter") {

  # turn this into a suitable list of our parameters
  pars_list <- split(pars[[value]], pars[[parameter]])
  for (i in seq_along(pars_list)) {
    if (suppressWarnings(!is.na(as.numeric(pars_list[[i]])))) {
      pars_list[[i]] <- as.numeric(pars_list[[i]])
    }
  }

  return(pars_list)

}

#' @noRd
#' @keywords internal
quiet_message <- function(msg) {

  if(Sys.getenv("SAMRAT_LOUD") == "TRUE") {
    message(msg)
  }

}
