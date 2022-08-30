`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


#------------------------------------------------
#' read_samrat_file
#'
#' \code{read_samrat_file} read samrat package file
#'
#' @description Load a file from within the inst/extdata folder of the
#'   ICMDMM package. File extension must be one of .csv, .rds, or .xlsx
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
  if(is.element(ext, c("csv", "rds", "xlsx")) == FALSE){
    stop("file extension not valid")
  }

  # get full file path
  name_full <- system.file("extdata/", name, package="samrat", mustWork = TRUE)

  # read in file
  if (ext == "rds") {
    ret <- readRDS(name_full)
  } else if (ext == "csv") {
    ret <- utils::read.csv(file=name_full, header=TRUE, sep=",")
  } else {
    ret <- readxl::read_xlsx(name_full, sheet = sheet)
  }

  return(ret)

}
