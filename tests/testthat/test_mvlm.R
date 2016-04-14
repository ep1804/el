context("Multivariate Linear Regression")

test_that("MVLM functions", {
  
  tr <- bearing[1:12000,-1]
  ob <- bearing[-(1:12000),-1]
  
  m <- el.mvlm(tr, alpha = 0.01)
  s <- el.mvlmScore(ob, m$fit)
  
  expect_true(abs(sum(s$est) + 10950.45) < 1)
})
