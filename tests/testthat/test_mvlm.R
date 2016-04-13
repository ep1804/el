context("Multivariate Linear Regression")

test_that("MVLM functions", {
  data = bearing[,-1]
  half = as.integer(nrow(data) / 2)
  
  mdl <- el.mvlm(data[1:half,])
  
  sc <- el.mvlmScore(data[-(1:half),], mdl$fit)
  
  expect_true(nrow(data) == 2156) # FIXME real test needed
})
