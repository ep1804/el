#' Auto Correlation Function
#'
#' @param data   vector.
#' @param alpha  numeric. Target critical index (both tails)
#' @param plot   logical. Plot or not
#'
#' @return list(lag, )
#' @export
#'
#' @examples el.acf(beaver1$temp, plot=TRUE)
#' 
el.acf <- function(data, alpha = 0.05, plot = TRUE) {
  
  if (!el.isValid(data, 'single')) return()
  
  a <- acf(data, plot = plot, na.action = na.pass)
  
  ucl <- NULL
  lcl <- NULL
  
  lag <- a$lag[, 1, 1]
  N <- a$n.used
  for (k in lag) {
    ucl <- c(ucl, (-1 + qnorm(1 - alpha / 2) * sqrt(N - k - 1)) / (N - k))
    lcl <- c(lcl, (-1 + qnorm(alpha / 2) * sqrt(N - k - 1)) / (N - k))
  }
  
  list(
    lag = lag,
    acf = a$acf[, 1, 1],
    alpha = alpha,
    ucl = ucl,
    lcl = lcl
  )
}