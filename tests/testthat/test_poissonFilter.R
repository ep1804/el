context("Poisson Filter")

test_that("Poisson Filter functions", {
  
  bear <- bearing[seq(1, 20000, 10),]
  
  tr <- bear[1:1000, -1]
  ob <- bear[1001:2000, -1]
  
  m <- el.mvrLm(tr, alpha = 0.05)
  s <- el.mvrLmScore(ob, m$fit)
  
  s2 <- el.poissonFilter(s$alert)
  
  el.plot.est(ob, s$est, s2$score, rows = 4)
  
  expect_true(abs(sum(s2$score) - 70) < 10)
})
