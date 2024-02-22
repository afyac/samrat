#' Train, predict glmmTMB and return the result with boostraps
#'
#' @details Train, predict glm and return the result with boostraps
#' @param training_data Training data
#' @param predicting_data predicting data
#' @param resp_var reponse variable
#' @param admin1_col admin1 colnames
#' @param admin2_col admin2 colnames
#' @param name_output name to save the output
#' @export
f_pred_glm <- function(training_data, predicting_data,
                       pred_var, resp_var,
                       formula = fit_formula,
                       model = glm_fit,
                       admin1_col = 'region',
                       admin2_col = 'district',
                       name_output='som_glm_model'){

  ##Define the variables under study overall or under5
  if (resp_var %in% c("cdr", "n_died")) {resp_var <- "cdr"}
  if (resp_var %in% c("cdr_u5", "n_died_u5")) {resp_var <- "cdr_u5"}

  p_time_var <- f_define_p_time(resp_var)
  pred_value <- f_define_pred_value(resp_var)
  pop_var <- f_define_pop_var(resp_var)
  annot <- f_define_annot_value(resp_var)

  # Fit summary and save them
  summary <- parameters::model_parameters(glm_fit, ci_method = "wald", exponentiate = TRUE)
  x <- flextable::flextable(insight::format_table(summary))
  x <- flextable::autofit(x)
  flextable::save_as_docx(x, path = paste('08_define_final_model/output/',
                                          name_output, '_summary',
                                          annot, '.docx',sep=""))

  ##Plot the country level curve and save it
  predicting_data[, p_time_var] <- 1
  predicting_data[, c('qualityScore')] <- 1

  ##Boostraping part
  #Firt pred and standard error
  pred <- predict(glm_fit,
                  predicting_data[, c(p_time_var,   pred_var, admin1_col,
                                      admin2_col, 'qualityScore')])
  se <- predict(glm_fit,
                predicting_data[, c(p_time_var,  pred_var, admin1_col,
                                    admin2_col, 'qualityScore')], se.fit=TRUE)$se.fit

  #Save pred and se
  predicting_data <- as.data.frame(predicting_data)
  predicting_data$pred <- pred
  predicting_data$se <- se
  predicting_data$date <- anytime::anydate(paste(predicting_data$year, predicting_data$month,sep="-")) # date format

  #Boostrap pred
  # Be careful of the order -- as we need to have the same between res_pop and predicting data
  predicting_data <- predicting_data[order(predicting_data[,admin2_col]), ]
  predicting_data <- predicting_data[order(predicting_data$date), ]

  #Without CI population
  bootstrap_pred <- suppressWarnings(
    replicate(1000,
              exp(
                {
                  rnorm(
                    nrow(predicting_data),
                    predicting_data$pred,
                    predicting_data$se
                  )
                }
              ) *predicting_data[, c(pop_var)]*lubridate::days_in_month(predicting_data$date)
    )
  )

  bootstrap_pred <- t(apply(bootstrap_pred, 1, function(x) {if (all(is.na(x))) return(x) else return(sort(x)) }))

  predicting_data <- data.frame(predicting_data, bootstrap_pred) |>
    dplyr::select(-p_time_var)

  ##Return the data with boostrapping
  return(predicting_data)
}

#' Generate the actual mean, low and upp dr and toll using a agg level
#'
#' @details Generate the actual mean, low and upp dr and toll using a agg level
#' @param boostrapping_results Boostraping results
#' @param agg_level Aggregate levels
#' @param nb_boostrap number of boostrapings
#' @param resp_var response variable
#' @export
f_generate_final_results <- function(boostrapping_results, agg_level, nb_boostrap, resp_var){
  ##Define the variables under study overall or under5
  if (resp_var %in% c("cdr", "n_died")) {resp_var <- "cdr"}
  if (resp_var %in% c("cdr_u5", "n_died_u5")) {resp_var <- "cdr_u5"}

  p_time_var <- f_define_p_time(resp_var)
  pred_value <- f_define_pred_value(resp_var)
  pop_var <- f_define_pop_var(resp_var)
  annot <- f_define_annot_value(resp_var)

  #Compute ptime if wanted
  boostrapping_results[, p_time_var] <- boostrapping_results[, pop_var]*lubridate::days_in_month(boostrapping_results$date)

  # Aggregate the boostrapping results per agg level
  if(length(agg_level) == 1){
    D_a <- aggregate(boostrapping_results[, c(paste('X', seq(1:nb_boostrap), sep="")) ] ,
                     by = list(boostrapping_results[, agg_level]), FUN = sum )
  }else{
    D_a <- aggregate(boostrapping_results[, c(paste('X', seq(1:nb_boostrap), sep="")) ] ,
                     by = boostrapping_results[, agg_level], FUN = sum )
  }

  # Aggreagte the p_time results per agg level
  if(length(agg_level) == 1){
    pred_agg_region_ptime <- aggregate(boostrapping_results[, c(p_time_var)] ,
                                       by = list(boostrapping_results[, agg_level]), FUN = sum )
  }else{
    pred_agg_region_ptime <- aggregate(boostrapping_results[, c(p_time_var)] ,
                                       by = boostrapping_results[, agg_level], FUN = sum )
  }
  colnames(pred_agg_region_ptime) <- c(agg_level, p_time_var)

  ## Calculate mean, q1 and q3 of the boostrapping on the actual matrix results
  D_a <- data.frame(t(apply(D_a[, c(paste('X', seq(1:nb_boostrap), sep=""))], 1,
                            FUN = function(x) {return(c(mean(x), quantile(x, c(0.025, 0.975))))} )))
  colnames(D_a) <-c('mean', 'q1', 'q3')
  D_a[, agg_level] <- pred_agg_region_ptime[, agg_level]

  # Calculate the dr rate
  D_a_dr <- data.frame(pred_agg_region_ptime[, agg_level])
  colnames(D_a_dr) <- agg_level
  D_a_dr$mean <- D_a$mean *10000/ pred_agg_region_ptime[, p_time_var]
  D_a_dr$q1 <- D_a$q1 *10000/ pred_agg_region_ptime[, p_time_var]
  D_a_dr$q3 <- D_a$q3 *10000/ pred_agg_region_ptime[, p_time_var]

  # Merge toll and dr
  D_a <- merge(D_a, D_a_dr, by=c(agg_level), all.x=TRUE, all.y=TRUE)
  colnames(D_a) <- c(agg_level, paste('toll_mean', annot, sep=""),
                     paste('toll_low', annot, sep=""),
                     paste('toll_up', annot, sep=""),
                     paste('dr_mean', annot, sep=""),
                     paste('dr_low', annot, sep=""),
                     paste('dr_up', annot, sep="")
  )
  return(D_a)
}
