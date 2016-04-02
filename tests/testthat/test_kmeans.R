context("Correlation matrix")

test_that("Check kmeans performance result of iris[,1]", {
  d <- as.vector(iris[,1])
  kf <- el.kmeansPerf(iris[,1:4], plot=F)
  
  expect_true(is.numeric(kf))
  expect_equal(length(kf), 20)
})

test_that("Check kmeans performance result of iris[,1:4]", {
  d <- iris[,1:4]
  kf <- el.kmeansPerf(iris[,1:4], plot=F)
  
  expect_true(is.numeric(kf))
  expect_equal(length(kf), 20)
})

test_that("Check kmeans result of iris[,1:4]", {
  d <- iris[,1:4]
  cl <- el.kmeans(d, 3)
  score <- el.kmeansScore(d, cl$model)
  
  expect_equal(cl$score, score)
})
