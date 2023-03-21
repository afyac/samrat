#' @title Robust Standard Errors
#'
#' @description This function calculates robust standard errors for model coefficients, given a robust variance-covariance matrix.
#'
#' @param model_fit The fitted model object.
#' @param varcov The robust variance-covariance matrix.
#' @param scale A character string indicating the scale of the model coefficients.
#' Possible values are "linear" (default) or "log".
#' @return A data frame with the following columns:
#' \itemize{
#' \item Rate ratio: The estimate of the model coefficient on the specified scale.
#' \item 95%CI - lower: The lower bound of the 95% confidence interval for the estimate.
#' \item 95%CI - upper: The upper bound of the 95% confidence interval for the estimate.
#' \item Pr(>|z|): The p-value for the significance of the estimate.
#' }
#' @examples
#' library(sandwich)
#' data(mtcars)
#'
#' # Build model
#' model_fit <- lm(mpg ~ cyl + disp, data = mtcars)
#'
#' # Calculate the robust variance-covariance matrix
#' varcov <- vcovHC(model_fit, type = "HC")
#'
#' # Estimate robust standard errors
#' robust_se(model_fit, varcov, scale = "linear")
#' @importFrom stats diag pnorm round
#' @importFrom stats exp coef pnorm
#' @importFrom base ifelse
#' @importFrom sandwich vcovHC
#' @importFrom dplyr rename
#' @importFrom data mtcars
#' @export

robust_se <- function(model_fit, varcov, scale) {
  # Check for negative variances
  if (any(diag(varcov) < 0)) {
    stop("Negative variance detected")
  }

  # Calculate robust standard errors
  std.err <- sqrt(diag(varcov))

  # Calculate rate ratio and confidence intervals
  if (scale == "log") {
    rate_ratio <- round(coef(model_fit), 3)
    ci_lower <- round(coef(model_fit) - 1.96 * std.err, 3)
    ci_upper <- round(coef(model_fit) + 1.96 * std.err, 3)
  } else {
    rate_ratio <- round(exp(coef(model_fit)), 3)
    ci_lower <- round(exp(coef(model_fit) - 1.96 * std.err), 3)
    ci_upper <- round(exp(coef(model_fit) + 1.96 * std.err), 3)
  }

  # Calculate p-value for significance of estimate
  p_value <- round(2 * pnorm(abs(coef(model_fit) / std.err), lower.tail = FALSE), 3)

  # Create output data frame
  r_est <- data.frame(rate_ratio, ci_lower, ci_upper, p_value) %>%
    rename(
      "Rate ratio" = rate_ratio, "Pr(>|z|)" = p_value,
      "95%CI - lower" = ci_lower, "95%CI - upper" = ci_upper
    )

  # Return output
  return(r_est)
}
