context("smals")

test_that("f_liv works", {
  lv_terms <- lapply(livelihood_substrings, "[[", 1)

  expect_equal(f_liv(lv_terms[[1]], livelihood_substrings), names(lv_terms)[1])
  expect_equal(f_liv(lv_terms[[2]], livelihood_substrings), names(lv_terms)[2])
  expect_equal(f_liv(lv_terms[[3]], livelihood_substrings), names(lv_terms)[3])
  expect_equal(f_liv(lv_terms[[4]], livelihood_substrings), names(lv_terms)[4])
  expect_equal(f_liv(lv_terms[[5]], livelihood_substrings), names(lv_terms)[5])
  expect_equal(f_liv(lv_terms[[6]], livelihood_substrings), names(lv_terms)[6])
  expect_equal(f_liv(lv_terms[[7]], livelihood_substrings), names(lv_terms)[7])
  expect_equal(f_liv(lv_terms[[8]], livelihood_substrings), names(lv_terms)[8])
  expect_equal(f_liv(lv_terms[[9]], livelihood_substrings), names(lv_terms)[9])
  expect_equal(f_liv("zxc", livelihood_substrings), NA)
})
