context("Correlation matrix")

test_that("Check correlation matrix result of iris[,1:4]", {
  co <- el.cor(iris[,-5], plot=F)
  
  expect_equal(round(sum(co), 5), 7.48085)
})
