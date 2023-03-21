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
