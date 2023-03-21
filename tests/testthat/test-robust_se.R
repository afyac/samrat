context("Test robust_se function")

# Dependencies and model set up -------------------------------------------

# Build model
model_fit <- lm(mpg ~ cyl + disp, data = mtcars)

# Calculate the robust variance-covariance matrix
varcov <- vcovHC(model_fit, type = "HC")

# Tests -------------------------------------------------------------------


# Test case 1: Test the output data frame has correct column names
test_that(
  "robust_se returns a data frame with correct column names",
  {
    result <- robust_se(model_fit, varcov, scale = "linear")
    expect_named(result, c("Rate ratio", "95%CI - lower", "95%CI - upper", "Pr(>|z|)"))
  }
)

# Test case 2: Test the output data frame has the correct number of rows
test_that(
  "robust_se returns a data frame with the correct number of rows",
  {
    result <- robust_se(model_fit, varcov, scale = "linear")
    expect_equal(nrow(result), length(coef(model_fit)))
  }
)

# Test case 3: Test the output data frame has the correct number of columns
test_that(
  "robust_se returns a data frame with the correct number of columns",
  {
    result <- robust_se(model_fit, varcov, scale = "linear")
    expect_equal(ncol(result), 4)
  }
)

# Test case 4: Test that the function returns an error when there is a negative variance
test_that(
  "robust_se returns an error when there is a negative variance",
  {
    varcov_neg <- varcov
    diag(varcov_neg)[1] <- -1
    expect_error(robust_se(model_fit, varcov_neg, scale = "linear"), "Negative variance detected")
  }
)


# Test case 5: Test that the function works correctly for both "linear" and "log" scales
test_that(
  "robust_se works correctly for both linear and log scales",
  {
    result_linear <- robust_se(model_fit, varcov, scale = "linear")
    result_log <- robust_se(model_fit, varcov, scale = "log")

    # Check that the rate ratios are exponentiated in the linear scale
    expect_equal(result_linear$`Rate ratio`, unname(round(exp(coef(model_fit)), 3)))

    # Check that the rate ratios are not exponentiated in the log scale
    expect_equal(result_log$`Rate ratio`, unname(round(coef(model_fit), 3)))
  }
)
