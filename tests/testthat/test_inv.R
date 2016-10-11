context("Matrix inverse")

test_that("Matrix inverse", {
  d <- cov(iris[,1:4])
  iv <- el.inv(d)
  
  expect_equal(round(sum(iv), 4), 14.1148)
})

test_that("Matrix pseudo inverse", {
  set.seed(1)
  
  d <- matrix(rep(1, 16), nrow=4)
  iv <- el.inv(d)
  
  expect_equal(round(sum(iv), 5), 1)
})