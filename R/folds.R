#' Calculate folds
#'
#' @details Calculate folds
#' @param training_data Training Dataset
#' @param admin2_col Colnames of admin2
#' @export
f_calculate_folds <- function(training_data, admin2_col,
                              k_folds = 10){
  training_data <- training_data |>
    dplyr::mutate(fold_unit = paste(admin2_col,
                                    cut(time_unit,
                                        breaks = c(seq(0, max(time_unit), by = 3)) ), sep="_"))
  fold_units <- sort(unique(training_data$fold_unit))
  shuffle <- data.frame(fold_unit = fold_units,
                        new_rank = sample(1:length(fold_units),
                                          length(fold_units),
                                          replace = FALSE))
  concat_folds <- split(shuffle$new_rank, sort(shuffle[, "new_rank"]) %% k_folds)
  names(concat_folds) <- paste("fold", names(concat_folds), sep = "")
  splits <- data.frame(fold_k = rep(names(concat_folds), sapply(concat_folds, length)),
                       new_rank = unlist(concat_folds))
  shuffle <- merge(shuffle, splits, by = "new_rank")
  # Attribute folds to data, and order by fold
  training_data <- merge(training_data, shuffle, by = "fold_unit", all.x = TRUE)
  training_data <- training_data[order(training_data$fold_k), ]

  return(training_data)
}






