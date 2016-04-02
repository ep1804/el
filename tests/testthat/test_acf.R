context("Auto-correlation function")

test_that("Check acf result of beaver1 time series", {
  a <- el.acf(beaver1$temp, plot=F)
  expect_true(abs(sum(a$acf) - 4.607539) < 1E-6)
})
