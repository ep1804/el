context("Control limits")

test_that("Check limit functions", {
  data <- rnorm(1000) * 100 + 50
  
  zucl <- el.zlimit(data)
  zlcl <- el.zlimit(data, upper = F)
  
  bucl <- el.limit(data)
  blcl <- el.limit(data, upper = F)
  
  expect_true(abs(bucl - zucl) < 20)
  expect_true(abs(blcl - zlcl) < 20)
})
