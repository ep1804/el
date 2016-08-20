context("K-Means clustering")

test_that("Check kmeans performance result of iris[,1]", {
  kf <- el.kmeansPerf(iris[,1:4])
  
  expect_true(is.numeric(kf))
  expect_equal(length(kf), 15)
})

test_that("Check kmeans performance result of iris[,1:4]", {
  kf <- el.kmeansPerf(iris[,1:4])
  
  expect_true(is.numeric(kf))
  expect_equal(length(kf), 15)
})

test_that("Check kmeans result of iris[,1:4]", {
  d <- iris[,1:4]
  cl <- el.kmeans(d, 3)
  score <- el.kmeansScore(d, cl$fit)
  
  expect_equal(cl$score, score)
})
