context("Multivariate Regression with SVR Model")

test_that("MVR SV functions", {
  set.seed(1)
  
  tr <- bearing[1:(nrow(bearing)/2), -1]
  ob <- bearing[, -1]
  
  model <- el.mvrSv(tr, alpha = 0.05)
  score <- el.mvrSvScore(ob, model$fit)
 
  expect_true(abs(sum(score) / -6.502692 - 1) < 1E-4)
})