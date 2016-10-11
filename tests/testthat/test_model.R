context("caret Modeling")

test_that("Check regression modeling", {
  requireNamespace('ggplot2')
  
  set.seed(1)
  
  dia <- ggplot2::diamonds
  dia <- dia[sample(1:nrow(dia), 800),]
  y <- dia$price
  x <- dia
  x$price <- NULL
  
  evaluate_promise({
    fits <- el.model(y, x)
    cat(max(fits$LM$results$Rsquared))
  }, print = T)
  
  expect_true(is.list(fits))
  expect_true(length(fits$LM$results$Rsquared) == 9)
  
  # FIXME random seed not works?
  expect_true(abs(max(fits$LM$results$Rsquared) / 0.889758 - 1) < 1E-4) 
})

