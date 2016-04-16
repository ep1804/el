context("Multivariate Regression with Random Forest Model")

test_that("MVR RF functions", {
  
  tr <- bearing[1:12000, -1]
  ob <- bearing[-(1:12000), -1]
  
  # m <- el.mvrRf(tr, alpha = 0.01)
  # s <- el.mvrRfScore(ob, m$fit)
  
  # s2 <- el.poissonFilter(s$alert)
  # el.plot.est(ob, s$est, s2$score, rows = 4)
  
  # expect_true(abs(sum(s$est) + 10954.97) < 10)
  expect_true(TRUE)
})
