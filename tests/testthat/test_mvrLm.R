context("Multivariate Regression with Linear Model")

test_that("MVR LM functions", {
  
  tr <- bearing[1:(nrow(bearing)/2), -1]
  ob <- bearing[, -1]
  
  m <- el.mvrLm(tr, alpha = 0.01)
  s <- el.mvrLmScore(ob, m$fit)
  
  expect_true(TRUE)
})
