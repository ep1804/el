context("Multivariate Linear Regression")

test_that("MVLM functions", {
  data <- bearing
  
  expect_true(nrow(data) == 2156)
})
