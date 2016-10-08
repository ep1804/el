context("K-Means clustering")

test_that("Check kmeans result of iris[,1:4]", {
  set.seed(1)
  
  d <- iris[,-5]
  cl <- el.kmeans(d)
  score <- el.kmeansScore(d, cl$fit)
  
  expect_equal(cl$score, score)
})
