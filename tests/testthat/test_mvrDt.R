context("Multivariate Regression with Decision Tree Model")

test_that("MVR DT functions", {
  
  tr <- bearing[1:(nrow(bearing)/2), -1]
  ob <- bearing[, -1]
  
  model <- el.mvrDt(tr, alpha = 0.05)
  score <- el.mvrDtScore(ob, model$fit)
  
  score2 <- el.poissonFilter(score$alert)
  el.plot.est(ob, score$est, score2$score, rows = 4)
  
  expect_true(TRUE) # TODO
})
