#' Function to generate counterfactuals
#'
#' @details Function to generate counterfactuals
#' @param boostrapping_results Datasets containing boostraping
#' @param resp_var response variable
#' @param agg_level List of levels to aggregate
#' @param start_date_per_1 Start date period 1
#' @param end_date_per_1 End date period 1
#' @param start_date_per_2 Start date period 2
#' @param end_date_per_2 End date period 2
#' @param nb_boostrap number of boostrapping
#' @param type_model type of model
#' @param admin2_col colnames of admin2
#' @export

f_counterfactuals <- function(boostrapping_results, resp_var,
                              agg_level, admin2_col,
                              start_date_per_1 = "2015-01-01",
                              end_date_per_1 = "2016-06-01",
                              start_date_per_2 = "2022-01-01",
                              end_date_per_2 = "2022-12-01",
                              nb_boostrap=1000, type_model = 'glm'){
  ##Define the variables under study overall or under5
  p_time_var <- f_define_p_time(resp_var)
  pred_value <- f_define_pred_value(resp_var)
  pop_var <- f_define_pop_var(resp_var)
  annot <- f_define_annot_value(resp_var)

  ##First Calculate the median (counterfactual) of the non_crisis period (or period 1)
  median_c <- boostrapping_results |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(admin2_col)))) |>
    dplyr::filter(date >= start_date_per_1 & date <= end_date_per_1) |>
    dplyr::mutate(median_c = mean(exp(pred))) |>
    dplyr::select(median_c, district) |>
    dplyr::distinct(district, .keep_all = TRUE) |>
    dplyr::ungroup()

  boostrapping_results <- merge(boostrapping_results, median_c,
                                by=c(admin2_col), all.x=TRUE)

  boostrapping_results$median_c <- boostrapping_results$median_c*boostrapping_results[, pop_var]*lubridate::days_in_month(boostrapping_results$date)

  ## Calculate the excess -- number of death crisis - number of death non crisis
  boostrapping_results <- subset(boostrapping_results, date >= start_date_per_2  & date <= end_date_per_2)
  boostrapping_results[, c(paste('X_e', seq(1:nb_boostrap), sep="")) ] <- boostrapping_results[, c(paste('X', seq(1:nb_boostrap), sep="")) ] - boostrapping_results$median_c

  #Compute ptime if wanted
  boostrapping_results[, p_time_var] <- boostrapping_results[, pop_var]*lubridate::days_in_month(boostrapping_results$date)

  # D_e --  Matrix of excess death -- the number
  if(length(agg_level) == 1){
    D_e <- aggregate(boostrapping_results[, c(paste('X_e', seq(1:nb_boostrap), sep="")) ] ,
                     by = list(boostrapping_results[, agg_level]), FUN = sum )
  }else{
    D_e <- aggregate(boostrapping_results[, c(paste('X_e', seq(1:nb_boostrap), sep="")) ] ,
                     by = boostrapping_results[, agg_level], FUN = sum )
  }

  colnames(D_e)[1:length(agg_level)] <- agg_level

  if(length(agg_level) == 1){
    pred_agg_region_ptime <- aggregate(boostrapping_results[, c(p_time_var)] ,
                                       by = list(boostrapping_results[, agg_level]), FUN = sum )
  }else{
    pred_agg_region_ptime <- aggregate(boostrapping_results[, c(p_time_var)] ,
                                       by = boostrapping_results[, agg_level], FUN = sum )
  }

  colnames(pred_agg_region_ptime) <- c(agg_level, p_time_var)

  ## Calculate mean, q1 and q3 of the boostrapping on D_e
  D_e <- data.frame(t(apply(D_e[, c(paste('X_e', seq(1:nb_boostrap), sep=""))], 1,
                            FUN = function(x) {return(c(mean(x), quantile(x, c(0.25, 0.75))))} )))
  colnames(D_e) <- c('mean', 'q1', 'q3')
  D_e[, agg_level]<- pred_agg_region_ptime[, agg_level]

  # Calculate dr
  D_e_dr <- data.frame(pred_agg_region_ptime[, agg_level])
  colnames(D_e_dr) <- agg_level
  D_e_dr$mean <- D_e$mean *10000/ pred_agg_region_ptime[, p_time_var]
  D_e_dr$q1 <- D_e$q1 *10000/ pred_agg_region_ptime[, p_time_var]
  D_e_dr$q3 <- D_e$q3 *10000/ pred_agg_region_ptime[, p_time_var]

  D_e <- merge(D_e, D_e_dr, by=c(agg_level), all.x=TRUE, all.y=TRUE)
  colnames(D_e) <- c(agg_level, paste('toll_excess_period', annot, sep=''), paste('toll_excess_period', annot, '_low', sep=''),
                     paste('toll_excess_period', annot, '_up', sep=''), paste('dr_excess_period', annot, sep=''),
                     paste('dr_excess_period', annot, '_low', sep=''), paste('dr_excess_period', annot, '_up', sep='') )
  return(D_e)
}
