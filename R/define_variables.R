#' Define p_time variable
#'
#' @details Define p_time variable
#' @param resp_var responsive variable
#' @export
f_define_p_time <- function(resp_var){
  if(resp_var == 'cdr' | resp_var == 'n_died'){
    p_time_var <- 'p_time'
  }else{
    p_time_var <- 'p_time_u5'
  }
}

#' Define pop_var variable
#'
#' @details Define pop variable
#' @param resp_var responsive variable
#' @export
f_define_pop_var <- function(resp_var){
  if(resp_var == 'cdr' | resp_var == 'n_died'){
    pop_var <- 'pop_average'
  }else{
    pop_var <- 'pop_average_u5'
  }
}

#' Define pred values variable
#'
#' @details Define pred_value variable
#' @param resp_var responsive variable
#' @export
f_define_pred_value <- function(resp_var){
  if(resp_var == 'cdr' | resp_var == 'n_died'){
    pred_value <- 'n_died'
  }else{
    pred_value <- 'n_died_u5'
  }
}

#' Define annot variable
#'
#' @details Define annot variable
#' @param resp_var responsive variable
#' @export
f_define_annot_value <- function(resp_var){
  if(resp_var == 'cdr' | resp_var == 'n_died'){
    annot <- ''
  }else{
    annot <- '_u5'
  }
}
