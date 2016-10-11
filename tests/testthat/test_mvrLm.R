context("Multivariate Regression with Linear Model")

test_that("MVR LM functions", {
  set.seed(1)
  
  tr <- bearing[1:(nrow(bearing)/2), -1]
  ob <- bearing[, -1]
  
  model <- el.mvrLm(tr, alpha = 0.05)
  score <- el.mvrLmScore(ob, model$fit)
  
  expect_equal(round(sum(score), 6), 0.875203)
})