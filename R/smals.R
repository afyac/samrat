#' \code{f_liv}
#' @description  Function to standardise livelihood type nomenclature
#'
#' @param liv_term Livelihood liv_term being matched with
#' @param liv_ss Livelihood substrings
#' @keywords internal

f_liv <- function(liv_term, liv_ss) {

  # Livelihood
  if (grepl(paste(liv_ss$livelihood, collapse = "|"), liv_term)) {
    return(paste(names(livelihood_substrings)[1]))
  }

  # Agriculturalists
  if (grepl(paste(liv_ss$agriculturalists, collapse = "|"), liv_term)) {
    return(paste(names(livelihood_substrings)[2]))
  }

  # Pastoralists (slightly different to avoid confusion with agropastoralists)
  if (grepl(paste(liv_ss$pastoralists, collapse = "|"), liv_term) &&
      !grepl(paste(liv_ss$agropastoralists, collapse = "|"), liv_term)) {
    return(paste(names(livelihood_substrings)[3]))
  }

  # Agropastoralists
  if (grepl(paste(liv_ss$agropastoralists, collapse = "|"), liv_term)) {
    return(paste(names(livelihood_substrings)[4]))
  }

  # Riverine
  if (grepl(paste(liv_ss$riverine, collapse = "|"), liv_term)) {
    return(paste(names(livelihood_substrings)[5]))
  }

  # Fishing
  if (grepl(paste(liv_ss$fishing, collapse = "|"), liv_term)) {
    return(paste(names(livelihood_substrings)[6]))
  }

  # Urban
  if (grepl(paste(liv_ss$urban, collapse = "|"), liv_term)) {
    return(paste(names(livelihood_substrings)[7]))
  }

  # Displaced
  if (grepl(paste(liv_ss$displaced, collapse = "|"), liv_term)) {
    return(paste(names(livelihood_substrings)[8]))
  }

  # Refugee
  if (grepl(paste(liv_ss$refugee, collapse = "|"), liv_term)) {
    return(paste(names(livelihood_substrings)[9]))
  }

  # Otherwise return NA
  if (!grepl(paste(unlist(liv_ss), collapse = "|"), liv_term)) {
    return(NA)
  }

}

#' Create timeseries for analysis
#'
#' \code{samrat_timeseries} Create timeseries for analysis in samrat
#'
#' @description Uses parameters and strata to generate timeseries for mortality
#'   inference.
#'
#' @param strata Strata output from [samrat_read_strata()]
#' @param pars_list Parameter outputs from [samrat_read_params()]
#' @return Returns a list of timeseries objects including:
#' \itemize{
#'       \item{"ts"}{"Time series `data.frame`"}
#'       \item{"tm_list"}{"Time parameters `list`"}
#' }
#' @export
#' @examples {
#'
#' param_file <- system.file(
#' "extdata/som_analysis_parameters.xlsx", package="samrat"
#' )
#' pars_list <- samrat_read_params(param_file)
#'
#' strata_file <- system.file(
#'   "extdata/som_analysis_strata.xlsx", package="samrat"
#' )
#' strata <- samrat_read_strata(strata_file, pars_list)
#'
#' tm_list <- samrat_timeseries(pars_list, strata)
#' names(tm_list)
#'
#' }
samrat_timeseries <- function(pars_list, strata) {

  assert_custom_class(pars_list, "samrat_params")
  assert_custom_class(strata, "samrat_strata")
  pl <- pars_list$pars_list

  #...................................
  ## Create a time series of stratum-time
  ## (including burn-in/-out periods of x years before/after analysis period)

  # create a time unit variable tm (from month 1 to month T of analysis period T)
  mth_T <- pl$y_analysis_end + pl$burn_out_period -
    pl$y_analysis_start + pl$burn_in_period
  tm <- seq(1, ((mth_T * 12) + pl$m_analysis_end - pl$m_analysis_start + 1 ), 1)

  # create a time series of stratum-year-months
  ts <- expand.grid(strata$stratum, tm)
  colnames(ts) <- c("stratum", "tm")

  # work out corresponding year
  ts$y <- floor((ts[, "tm"] + pl$m_analysis_start - 2) / 12)
  ts$y <- ts$y + pl$y_analysis_start - pl$burn_in_period

  # and the month values
  ts$m <- (ts[, "tm"] + pl$m_analysis_start - 1)
  ts$m <- ts$m - (ts[, "y"] - pl$y_analysis_start + pl$burn_in_period) * 12

  # merge admin1 back in
  ts <- merge(
    ts,
    strata[, c("stratum", "admin1")], by = c("stratum"), sort = TRUE
  )

  # sort time series
  ts <- ts[order(ts[, "stratum"], ts[, "tm"]), ]


  #...................................
  ## Define stratum names and time units
  # Stratum names
  stratum_names <- as.character(unique(strata[, "stratum"]))
  stratum_names <- sort(stratum_names)

  # Time units
  t_units <- unique(ts[, c("tm", "m", "y")])

  # Period start and end points
  # overall period of analysis
  tm_analysis_start <- t_units %>%
    filter(.data$y == pl$y_analysis_start & .data$m == pl$m_analysis_start) %>%
    pull(tm)
  tm_analysis_end <- t_units %>%
    filter(.data$y == pl$y_analysis_end & .data$m == pl$m_analysis_end) %>%
    pull(tm)

  # period over which excess mortality is to be estimated
  tm_excess_start <- t_units %>%
    filter(.data$y == pl$y_excess_start & .data$m == pl$m_excess_start) %>%
    pull(tm)

  tm_excess_end <- t_units %>%
    filter(.data$y == pl$y_excess_end & .data$m == pl$m_excess_end) %>%
    pull(tm)

  # bring all the time pars into a list
  time_list <- list(
    "tm_analysis_start" = tm_analysis_start,
    "tm_analysis_end" = tm_analysis_end,
    "tm_excess_start" = tm_excess_start,
    "tm_excess_end" = tm_excess_end
  )

  #...................................
  ## and list to return
  tm_list <- list(
    "ts" = ts,
    "time_list" = time_list
  )
  class(tm_list) <- c(class(tm_list), "samrat_params")
  return(tm_list)

}
