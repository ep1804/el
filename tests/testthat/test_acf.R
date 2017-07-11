context("Auto-correlation function")

test_that("Check acf result of beaver1 time series", {
  a <- el.acf(beaver1$temp, plot=F)
  
  expect_equal(round(sum(a$acf), 5), 4.60754)
})
