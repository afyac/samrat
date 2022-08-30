#------------------------------------------------
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
samrat_read_params <- function(param_file) {

  # check file is suitable name and exists
  assert_string(param_file)
  assert_file_exists(param_file)

  #...................................
  ## General parameters
  gen_pars <- readxl::read_excel(param_file, sheet = "general_parameters")

  # turn this into a suitable list of our parameters
  gen_pars_list <- split(gen_pars$value, gen_pars$parameter)
  for(i in seq_along(gen_pars_list)) {
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
  ## Sensitivity analysis parameters
  sens_pars <- readxl::read_excel(param_file, sheet = "sensitivity_parameters")
  sens_pars <- as.data.frame(sens_pars)

  # Bring all the parameters into one list
  pars <- list(
    "gen_pars" = gen_pars_list,
    "var_pars" = var_pars,
    "cf_pars"= cf_pars
  )

  return(pars)

}
