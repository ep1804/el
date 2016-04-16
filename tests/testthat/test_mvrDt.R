context("Multivariate Regression with Decision Tree Model")

test_that("MVR DT functions", {
  
  tr <- bearing[1:12000, -1]
  ob <- bearing[-(1:12000), -1]
  
  m <- el.mvrDt(tr, alpha = 0.01)
  s <- el.mvrDtScore(ob, m$fit)
  
  # s2 <- el.poissonFilter(s$alert)
  # el.plot.est(ob, s$est, s2$score, rows = 4)
  
  expect_true(abs(sum(s$est) + 10958.68) < 10)
})
