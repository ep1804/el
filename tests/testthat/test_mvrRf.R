context("Multivariate Regression with Random Forest Model")

test_that("MVR RF functions", {
  
  tr <- bearing[1:(nrow(bearing)/2), -1]
  ob <- bearing[, -1]
  
  model <- el.mvrRf(tr, alpha = 0.01)
  score <- el.mvrRfScore(ob, model$fit)
  
  expect_true(abs(sum(score) - 1.1) < 1.0)
})
