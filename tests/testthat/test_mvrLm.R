context("Multivariate Regression with Linear Model")

test_that("MVR LM functions", {
  
  tr <- bearing[1:(nrow(bearing)/2), -1]
  ob <- bearing[, -1]
  
  model <- el.mvrLm(tr, alpha = 0.05)
  score <- el.mvrLmScore(ob, model$fit)
  
  expect_true(abs(sum(score) - 0.8) < 0.5)
})