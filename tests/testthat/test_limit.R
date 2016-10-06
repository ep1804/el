context("Control limits")

test_that("Check limit functions", {
  set.seed(1)
  
  data <- rnorm(1000) * 100 + 50
  
  zucl <- el.zlimit(data)
  zlcl <- el.zlimit(data, upper = F)
  
  bucl <- el.limit(data)
  blcl <- el.limit(data, upper = F)
  
  expect_true(abs((bucl - zucl) / 4.688886 - 1) < 1E-4)
  expect_true(abs((blcl - zlcl) / -1.530813 - 1) < 1E-4)
})
