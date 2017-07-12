context("caret Regression Modeling")

test_that("Check regression modeling", {

  # http://r-pkgs.had.co.nz/check.html says:
  # Occasionally you may have a problem where the tests pass when run
  # interactively with devtools::test(), but fail when in R CMD check.
  # This usually indicates that you’ve made a faulty assumption about
  # the testing environment, and it’s often hard to figure it out.
  #
  # I had this situation: For some reason I don't know, set.seed()
  # didn't work and the final test failed. Than, I occationally
  # found the set.seed() didn't work just after restarting R session.
  # After some dansing, I've found a walkaround of inserting
  # following two requireNamespace() lines.
  #
  requireNamespace('caret')
  requireNamespace('caretEnsemble')

  set.seed(1234)

  x <- iris[,1:4]
  y <- as.numeric(iris$Species)
  fits <- el.model(x, y)

  expect_true(is.list(fits))
  expect_equal(round(fits$LM$pred$pred[1], 5), 1.05467)

  set.seed(1234)

  dia <- ggplot2::diamonds
  dia <- dia[sample(1:nrow(dia), 800),]
  x <- dia
  x$price <- NULL
  y <- dia$price
  fits <- el.model(x, y)

  expect_true(is.list(fits))
  expect_equal(round(fits$LM$pred$pred[1], 3), 4668.391)
})
