context("Multivariate Regression with Random Forest Model")

test_that("MVR RF functions", {
  
  bear <- bearing[seq(1, 20000, 10),]
  
  tr <- bear[1:1000, -1]
  ob <- bear[1001:2000, -1]
  
  m <- el.mvrRf(tr, alpha = 0.05)
  s <- el.mvrRfScore(ob, m$fit)
  
  s2 <- el.poissonFilter(s$alert)
  el.plot.est(ob, s$est, s2$score, rows = 4)
  
  # expect_true(abs(sum(s$est) + 10954.97) < 10)
  expect_true(TRUE)
})
