context("caret Modeling")

test_that("Check regression modeling", {
  requireNamespace('ggplot2')
  
  set.seed(1)
  
  dia <- ggplot2::diamonds
  dia <- dia[sample(1:nrow(dia), 800),]
  y <- dia$price
  x <- dia
  x$price <- NULL
  
  fits <- el.model(y, x)
  
  expect_true(is.list(fits))
  expect_true(length(fits$LM$results$Rsquared) == 9)
  #expect_true(abs(fits$LM$results$Rsquared[1] / 0.8759337 - 1) < 1E-4) # FIXME
  
})

