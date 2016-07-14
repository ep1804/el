context("Multivariate Regression with Decision Tree Model")

test_that("MVR DT functions", {
  
  bear <- bearing[seq(1, 20000, 10),]
  
  tr <- bear[1:1000, -1]
  ob <- bear[1001:2000, -1]
  
  m <- el.mvrDt(tr, alpha = 0.05)
  s <- el.mvrDtScore(ob, m$fit)
  
  s2 <- el.poissonFilter(s$alert)
  el.plot.est(ob, s$est, s2$score, rows = 4)
  
  expect_true(abs(sum(s$est) + 900) < 20)
})
