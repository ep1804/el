context("Data validator")

test_that("Accept numeric vectors", {
  expect_true(el.isValid(c(1, 2, 3, 4), 'single'))
  expect_true(el.isValid(c(1, 2, NA, 4), 'single'))
  expect_true(el.isValid(c(1, 2, NA, NA), 'single'))
})


test_that("Accept numeric matrix", {
  data = as.matrix(iris[1:10, 1:4])
  expect_true(el.isValid(data, 'multiple'))
  
  data[4:6, 1:3] <- NA
  expect_true(el.isValid(data, 'multiple'))
  
  data[1:10, 3:4] <- NA
  expect_true(el.isValid(data, 'multiple'))
})