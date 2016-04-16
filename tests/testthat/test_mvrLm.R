context("Multivariate Regression with Linear Model")

test_that("MVR LM functions", {
  
  tr <- bearing[1:12000, -1]
  ob <- bearing[-(1:12000), -1]
  
  m <- el.mvrLm(tr, alpha = 0.01)
  s <- el.mvrLmScore(ob, m$fit)
  
  # s2 <- el.poissonFilter(s$alert)
  # el.plot.est(ob, s$est, s2$score, rows = 4)
  
  expect_true(abs(sum(s$est) + 10950.45) < 10)
})
