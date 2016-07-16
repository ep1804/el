context("Multivariate Regression with Decision Tree Model")

test_that("MVR DT functions", {
  
  tr <- bearing[1:(nrow(bearing)/2), -1]
  ob <- bearing[, -1]
  
  model <- el.mvrDt(tr, alpha = 0.05)
  score <- el.mvrDtScore(ob, model$fit)
  
  expect_true(abs(sum(score) - 2.0) < 0.5)
})
