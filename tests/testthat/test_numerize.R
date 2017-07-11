context("Numerize data")

test_that("Matrix inverse", {

  dia <- ggplot2::diamonds

  dia2 <- el.numerize(dia)

  expect_equal(colnames(dia2)[4], "cutVery_Good")

})
