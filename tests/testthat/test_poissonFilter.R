context("Poisson Filter")

test_that("Poisson Filter functions", {
  
  tr <- bearing[1:(nrow(bearing)/2), -1]
  ob <- bearing[, -1]
  
  # m <- el.mvrLm(tr, alpha = 0.05)
  # s <- el.mvrLmScore(ob, m$fit)
  # 
  # s2 <- el.poissonFilter(s$alert)
  # 
  # el.plot.est(ob, s$est, s2$score, rows = 4)
  
  expect_true(TRUE)
})
