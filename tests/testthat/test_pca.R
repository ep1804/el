context("PCA")

test_that("Check PCA modeling", {
  data <- iris[,-5]
  pca <- el.pca(data)
  
  expect_true(is.matrix(pca$fit$loading))
  expect_true(abs(pca$fit$vaCusum[2] - 0.9776852) < 1E-5)
})

test_that("Check PCA scoring", {
  data <- iris[,-5]
  pca <- el.pca(data)
  score <- el.pcaScore(data, pca$fit)
  
  expect_true( abs(sum(pca$score - score)) < 1E-5 )
})

test_that("Check PCA unscoring", {
  data <- iris[,-5]
  pca <- el.pca(data)
  score <- el.pcaScore(data, pca$fit)
  d2 <- el.pcaUnscore(score, pca$fit)
  
  expect_true( abs(sum(data - d2)) < 1E-5 )
})