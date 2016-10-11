context("caret Modeling")

test_that("Check regression modeling", {
  
  #library(el)
  
  # http://r-pkgs.had.co.nz/check.html says:
  # Occasionally you may have a problem where the tests pass when run 
  # interactively with devtools::test(), but fail when in R CMD check. 
  # This usually indicates that you’ve made a faulty assumption about 
  # the testing environment, and it’s often hard to figure it out.
  # 
  # I had this situation: For some reason I don't know, set.seed() 
  # didn't work and final test failed. After some dansing, I've found 
  # a walkaround. That is inserting following two requireNamespace() 
  # lines.
  # 
  requireNamespace('caret')
  requireNamespace('caretEnsemble')
  
  set.seed(1)
  
  dia <- ggplot2::diamonds
  dia <- dia[sample(1:nrow(dia), 800),]
  y <- dia$price
  x <- dia
  x$price <- NULL
  
  fits <- el.model(y, x)
  
  expect_true(is.list(fits))
  expect_equal(length(fits$LM$results$Rsquared), 9)
  expect_equal(round(max(fits$LM$results$Rsquared), 5), 0.88976) 
})

