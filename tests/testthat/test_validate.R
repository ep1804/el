context("Data validator")

test_that("Accept numeric vectors", {
  expect_true(ear.isValid(c(1, 2, 3, 4), 'vector'))
  expect_true(ear.isValid(c(1, 2, NA, 4), 'vector'))
  expect_true(ear.isValid(c(1, 2, NA, NA), 'vector'))
})


test_that("Accept numeric matrix", {
  data = as.matrix(iris[1:10, 1:4])
  expect_true(ear.isValid(data, 'matrix'))
  
  data[4:6, 1:3] <- NA
  expect_true(ear.isValid(data, 'matrix'))
  
  data[1:10, 3:4] <- NA
  expect_true(ear.isValid(data, 'matrix'))
})