context("Matrix inverse")

test_that("Matrix inverse", {
  d <- cov(iris[,1:4])
  iv <- el.inv(d)
  
  expect_true(abs(sum(iv) - 14.11477) < 1E-5)
})

test_that("Matrix pseudo inverse", {
  set.seed(1)
  
  d <- matrix(rep(1, 16), nrow=4)
  iv <- el.inv(d)
  
  expect_true(abs(sum(iv) - 1) < 1E-6)
})