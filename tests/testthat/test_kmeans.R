context("K-Means clustering")

test_that("Check kmeans result of iris[,-5]", {
  set.seed(1)
  
  d <- iris[,-5]
  cl <- el.kmeans(d)
  score <- el.kmeansScore(d, cl$fit)
  
  expect_equal(cl$score, score)
})

test_that("Check kmeans result of iris[,-5]", {
  set.seed(1)
  
  d <- iris[,-5]
  cl <- el.kmeans(d, 3)
  score <- el.kmeansScore(d, cl$fit)
  
  expect_equal(cl$score, score)
})
