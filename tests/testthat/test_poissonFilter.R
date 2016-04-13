context("Poisson Filter")

test_that("Poisson Filter functions", {
  data = bearing[,-1]
  half = as.integer(nrow(data) / 2)
  
  mdl <- el.mvlm(data[1:half,])
  
  sc <- el.poissonFilter(mdl$score$alert)
  
  expect_true(nrow(data) == 2156) # FIXME real test needed
})
