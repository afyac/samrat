#' Function to generate feature selection
#'
#' @details Function to perform cross validation
#' @param list_pred_to_try list of predictor variables to try
#' @param pred_value value we want to predict
#' @param path path to save the dataset
#' @param training_data training dataset
#' @param which_fold_f Name of the fold used
#' @param admin2_col colnames of the admin2, if not used NULL
#' @param p_time_var p_time_var used or not (e.g NULL or p_time or p_time_u5)
#' @param model statistic model (e.g. glmmTB)
#' @export

f_feature_selection <- function(list_pred_to_try, pred_value,
                                path, training_data, which_fold_f,
                                admin2_col = NULL, p_time_var = NULL,
                                model = 'glmmTMB'){
  metrics <- c()
  list_preds <- c()
  for(nb_pred in seq(from = length(list_pred_to_try), to = 3)){
    list_preds_possible <- utils::combn(list_pred_to_try, m=as.integer(nb_pred))
    for(ind in 1:ncol(list_preds_possible)){
      select_preds <- c(list_preds_possible[, ind])
      print(paste('Feature values:', select_preds, sep=" "))

      formula <- paste(pred_value," ~",
                       paste(select_preds, collapse = " + "))
      if(length(admin2_col) != 0){
        formula <- paste(formula,'+','(1|', admin2_col, ')')
      }
      if(length(p_time_var) != 0){
        formula <- paste(formula, '+ offset(log(', p_time_var, '))')
      }
      fit_formula <- as.formula(formula)

      if(model == 'glmmTMB'){
        glm_fit <- glmmTMB::glmmTMB(formula = fit_formula, data = training_data,
                                    weights = qualityScore,family = "poisson")
      }else if(model == 'gam'){
        glm_fit <- mgcv::gam(formula = fit_formula, data = training_data,
                             weights = qualityScore, family = "poisson")
      }else if(model == 'glm'){
        glm_fit <- glm(formula = fit_formula, data = training_data,
                       weights = qualityScore, family = "poisson")
      }
      res <- f_cv(data_f = training_data, which_fold_f = which_fold_f,
                  k_folds_f = 10, fit_f = glm_fit,
                  f_family_f = f_family,
                  f_predict_f = f_predict, verbose = TRUE)

      metrics <- rbind(metrics, res$cv_metrics)
      list_preds <- rbind(list_preds, paste(c(select_preds), collapse=", "))
    }
  }

  results_final <- data.frame(metrics)
  results_final$variable <- list_preds

  utils::write.csv(results_final, path, row.names = FALSE)

}


