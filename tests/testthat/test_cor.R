context("Correlation matrix")

test_that("Check correlation matrix result of iris[,1:4]", {
  co <- el.cor(iris[,1:4], plot=F)
  expect_true(abs(sum(co) - 7.480849) < 1E-6)
})
