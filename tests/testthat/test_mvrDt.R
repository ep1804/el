context("Multivariate Regression with Decision Tree Model")

test_that("MVR DT functions", {
  set.seed(1)
  
  tr <- bearing[1:(nrow(bearing)/2), -1]
  ob <- bearing[, -1]
  
  model <- el.mvrDt(tr, alpha = 0.05)
  score <- el.mvrDtScore(ob, model$fit)
  
  expect_true(abs(sum(score) / 2.064863 - 1) < 1E-4)
})