#' \pkg{samrat} Small-area mortality analytics
#'
#' Package for conducting small-area estimation of
#' mortality and short term predictions.
#'
#' @docType package
#' @name samrat
#'
#' @importFrom stats setNames
#' @importFrom utils tail type.convert packageVersion unzip str capture.output
#' @importFrom utils read.csv download.file
#' @importFrom rappdirs user_cache_dir
#' @importFrom R6 R6Class
#' @importFrom storr storr_rds
#'
"_PACKAGE"

globalVariables(c("livelihood_substrings"))

# package variables
livelihood_substrings <- list(
  "livelihood" = c("livel", "lz", "LHZ", "lhz", "LZ", "Livel", "lhood", "Lhood"),
  "agriculturalists" = c("gric", "cultur", "farm", "cultiv", "Farm", "Cultur", "Cultiv"),
  "pastoralists" = c("past", "Past", "herd", "Herd", "cattle", "Cattle"),
  "agropastoralists" = c("grop", "grip", "groP", "griP", "gro P", "gri P", "gro-p", "gri-p"),
  "riverine" = c("river", "River", "riv", "Riv"),
  "fishing" = c("fish", "Fish"),
  "urban" = c("urb", "Urb", "city", "City", "town", "Town"),
  "displaced" = c("displ", "IDP", "camp", "intern", "Displ", "idp", "Camp", "PoC", "poc", "POC"),
  "refugee" = c("Ref", "ref", "gee")
)
