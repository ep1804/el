context("Control limits")

test_that("Check limit functions", {
  set.seed(1)
  
  data <- rnorm(1000) * 100 + 50
  
  zucl <- el.zlimit(data)
  zlcl <- el.zlimit(data, upper = F)
  
  bucl <- el.limit(data)
  blcl <- el.limit(data, upper = F)
  
  expect_equal(round(bucl - zucl, 5), 4.68889)
  expect_equal(round(blcl - zlcl, 5), -1.53081)
})
