#' Filtering model with Poision distribution
#'
#' @param data     logical matrix or data.frame. TRUE denotes arrival event
#' @param alpha    numeric. Critical level (pertains to upper limit of arrival numbers)
#' @param interval numeric. Interval size with which Poisson experiment is conducted
#' @param plot     logical. Plot or not
#'
#' @return list(logical vector. filtered one.
#' @export
#'
#' @examples   
#' tr <- bearing[1:12000, -1] 
#' ob <- bearing[-(1:12000), -1]  
#' m <- el.mvlm(tr, alpha = 0.01) 
#' s <- el.mvlmScore(ob, m$fit)  
#' s2 <- el.poissonFilter(s$alert)  
#' el.plot.est(ob, s$est, s2$score, rows = 4)
#' 
el.poissonFilter <- function(data, alpha = 0.05, interval = 50, plot = TRUE) {
    
  if (!is.logical(data)) {
    logger.error("Illegal argument: not a logical data")
    return()
  }
  
  d <- as.matrix(apply(data, c(1,2), as.integer))
  len <- nrow(d)
  
  if (len < interval) {
    logger.warn("Too small data")
    return()
  }
  
  # for each column, compute threshold of arrival numbers
  threshold <- apply(d, 2, function(x) {
    qpois(alpha, lambda = sum(x) * interval / len, lower.tail = FALSE)
  })
  
  arrivals <- apply(d, 2, function(x){
    sapply(1:len, function(i){ 
      if(x[i] > 0)
        sum(x[max(1, i - interval + 1):i])
      else
        0
    })
  })
  
  score <- t(apply(arrivals, 1, function(x){
    x > threshold
  }))
  
  list(
    fit = list(alpha = alpha,
               interval = interval,
               threshold = threshold),
    score = score
  )
}