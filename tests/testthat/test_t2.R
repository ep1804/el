context("T2")

test_that("Check T2 modeling", {
  set.seed(1)
  
  data <- iris[,-5]
  t2 <- el.t2(data, el.kmeans(data, 3)$fit)
  
  expect_equal(length(t2$fit$icov), 3)
  expect_true(is.matrix(t2$fit$icov[[1]]))
  expect_true(abs(sum(t2$score) / 588 - 1) < 1E-4)
})

test_that("Check T2 scoring", {
  set.seed(1)
  
  data <- iris[,-5]
  t2 <- el.t2(data, el.kmeans(data, 3)$fit)
  t2Score <- el.t2Score(data, t2$fit)
  
  expect_equal(length(t2Score), nrow(data))
  expect_true(abs(sum(t2Score) / 588 - 1) < 1E-4)
})