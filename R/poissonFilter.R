#' Filtering model with Poision distribution
#'
#' @param data     logical matrix or data.frame. TRUE denotes arrival event
#' @param interval numeric. Interval size with which Poisson experiment is conducted
#' @param alpha    numeric. Critical level (pertains to upper limit of arrival numbers)
#'
#' @return list(logical vector. filtered one.
#' @export
#'
#' @examples
#' 
el.poissonFilter <- function(data, alpha = 0.05, interval = 50) {
  
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
      sum(x[max(1, i - interval + 1):i])
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