#' Function to generate cross validation
#'
#' @details Function to perform cross validation
#' @param data_f Datasets containing the different folds
#' @param which_fold_f Which folds are we using
#' @param k_folds_f Number of folds
#' @param fit_f model fitting function
#' @param f_family_f family function of the fit (e.g poisson)
#' @export
f_cv <- function(data_f, which_fold_f, fit_f, f_predict_f,
                           f_family_f, k_folds_f = 10, verbose = TRUE) {

  # List of k folds
  folds_k <- unique(data_f$fold_k)

  #...................................
  ## More preparations

  # Prepare output
  # output 1 - by fold, to compute CV metrics
  out1 <- data.frame(fold_k = folds_k, obs = NA, pred = NA,
                     mse = NA, rmse = NA, bias_abs = NA, bias_rel = NA)

  # output 2 - by district-year, for plotting
  out2 <- c()

  # Identify response variable
  resp_var <- as.character(formula(fit_f))[2]

  #...................................
  ## For classes "glm", "glm.nb", "zeroinfl", "glmmTMB", "gam", "bam"
  if (any(class(fit_f) %in% c("glm", "glm.nb", "zeroinfl", "glmmTMB",
                              "gam", "bam") ) ) {

    # For each fold...
    for (i in 1:nrow(out1) ) {

      # progress
      if (verbose == TRUE) {print(paste("now working on fold ", i, " of ",
                                        nrow(out1), sep = ""))}

      # select training and holdout data
      df_train <- subset(data_f, fold_k != out1[i, "fold_k"])
      df_hold <- subset(data_f, fold_k == out1[i, "fold_k"])

      # fit on all data but the fold
      if (any(class(fit_f) %in% c("glm", "glm.nb", "zeroinfl", "glmmTMB") ) ) {
        cv_fit <- update(fit_f, data = df_train, formula. = fit_f$formula)
      }
      if (class(fit_f)[1] == "gam" ) {
        # figure out the weights variable(s)
        x <- names(getCall(fit_f))
        x <- as.character(getCall(fit_f))[which(x == "weights")]
        # re-fit
        cv_fit <- tryCatch(mgcv::gam(formula = formula(fit_f), data = df_train,
                               family = family(fit_f)$family, weights = eval(as.name(x)) ) )
      }
      if (class(fit_f)[1] == "bam" ) {
        # figure out the weights variable(s)
        x <- names(getCall(fit_f))
        x <- as.character(getCall(fit_f))[which(x == "weights")]
        # re-fit
        cv_fit <- tryCatch(mgcv::bam(formula(fit_f), data = df_train,
                               family = family(fit_f)$family, weights = eval(as.name(x)) ) )
      }
      ###FC: mgcv throws error on fitting the first fold
      # (there is *no* information about some basis coefficients)
      # for now have fixed with an error catcher

      if(inherits(cv_fit, "try-error")) {
        message(
          "ERROR: mgcv not fitting for this fold - execution continues");
        next}

      # predict on holdout fold
      df_hold$pred <- f_predict_f(fit_f = cv_fit, newdata_f = df_hold)

      # collect predictive metrics
      out1[i, "obs"] <- sum(df_hold[, resp_var])
      out1[i, "pred"] <- sum(df_hold$pred)
      out1[i, "mse"] <- mean((df_hold$pred - df_hold[, resp_var])^2)
      out1[i, "rmse"] <- sqrt(out1[i, "mse"])

      # track results by region-year
      x <- aggregate(df_hold[, c(resp_var, "pred")],
                     by = df_hold[, c("region", "year")], FUN = sum)
      x <- x[order(x$region, x$year), ]
      out2 <- rbind(out2, x)
    }
  }

  #...................................
  ## Return output

  # Compute additional metrics
  out1$bias_abs <- out1$pred - out1$obs
  out1$bias_rel <- out1$bias_abs / out1$obs
  out1 <- out1

  # Aggregate summary metrics across all folds
  out3 <- c(Matrix::colMeans(out1[, c("mse", "rmse", "bias_rel")]), sd(out1$rmse) )
  names(out3) <- c("mse", "rmse", "bias_rel", "sd_rmse")

  # Final aggregation of output 2 by region-year
  out2 <- aggregate(out2[, c(resp_var, "pred")],
                    by = out2[, c("region", "year")], FUN = sum)
  colnames(out2)[colnames(out2) == resp_var] <- "obs"

  # Return
  out <- list(out1, out2, out3, class(fit_f)[1], f_family_f(fit_f))
  names(out) <- c("cv_by_fold", "cv_by_region_year", "cv_metrics", "class",
                  "family")
  return(out)
}

#' Function to extract the distribution ("family") used to fit the model
#'
#' @details Function to extract the distribution ("family") used to fit the model
#' @param fit_f model fitting function
#' @export
f_family <- function(fit_f = fit) {

  #...................................
  ## Depending on the class of the fit object...

  if (class(fit_f)[1] == "glm") { return(family(fit_f)$family)}
  if (class(fit_f)[1] == "glm.nb") { return("negbin")}
  if (class(fit_f)[1] == "zeroinfl") { return(fit_f$dist)}
  if (class(fit_f)[1] == "glmmTMB") { return(fit_f$modelInfo$family$family)}
  if (class(fit_f)[1] %in% c("gam", "bam")) {return(family(fit_f)$family)}

}

#' Function to predict the dataset
#'
#' @details Function to predict the dataset
#' @param fit_f model fitting function
#' @param new_data_f validation data to predict
#' @param type_f type of prediction (e.g. response)
#' @param resp_var response variable
#' @param boostrap_f boostrapping function
#' @param n_bootstraps number of boostrapping
#' @examples
#' # example code
#'
#' @export
f_predict <- function(fit_f,
                      newdata_f, type_f = "response",
                      resp_var = NULL,
                      bootstrap_f = FALSE,
                      n_bootstraps = NULL) {

  # set seed
  set.seed(768)

  # Prediction for 'bn.fit' class
  if (class(fit_f)[1] == 'bn.fit') {
    newdata_f$pred <- predict(fit_f, node = resp_var,
                              data = newdata_f[, c(preds)],
                              method = 'bayes-lw', n = 50000)
    return(newdata_f$pred)
  }

  # Prediction for other classes
  if (any(class(fit_f) %in% c("glm", "glm.nb", "zeroinfl",
                              "glmmTMB", "gam", "bam"))) {

    # If desire point estimate
    if (!bootstrap_f) {
      return(predict(fit_f, newdata = newdata_f, type = type_f,
                     allow.new.levels = TRUE))
    }

    # If desire bootstrap random prediction
    est <- predict(fit_f, newdata = newdata_f, type = "link",
                   se.fit = TRUE, allow.new.levels = TRUE)
    out <- exp(replicate(n_bootstraps, rnorm(n = nrow(newdata_f),
                                             mean = est$fit, sd = est$se.fit)))
    out[is.nan(out)] <- NA
    out <- t(apply(out, 1, function(x) {
      if (all(is.na(x))) return(x) else return(sort(x))
    }))

    return(out)
  }
}


