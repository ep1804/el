#' Multivariate regression with vector distance model
#'
#' @param data  matrix or data.frame.   
#' @param alpha numeric. Critical level
#' @param plot  logical. Plot or not
#'
#' @return list(fit, score).
#' @export
#'
#' @examples
#' tr <- bearing[1:12000, -1] 
#' ob <- bearing[-(1:12000), -1]  
#' m <- el.mvrVd(tr, alpha = 0.01) 
#' s <- el.mvrVdScore(ob, m$fit)  
#' s2 <- el.poissonFilter(s$alert)  
#' el.plot.est(ob, s$est, s2$score, rows = 4)
#' 
el.mvrVd <- function(data, alpha = 0.05, plot = TRUE) {
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.matrix(data[complete.cases(data),])
  
  if (nrow(d) < ncol(d) * 4) {
    logger.error("Non-NA data is too small")
    return()
  }
  
  mtx = el.inv(t(d) %*% d) %*% t(d) %*% d
  est = d %*% mtx
  resi = d - est
  
  ucl = apply(resi, 2, function(x) {
    el.limit(x, alpha = alpha / 2)
  })
  
  lcl = apply(resi, 2, function(x) {
    el.limit(x, alpha = alpha / 2, upper = F)
  })
  
  alert = t(apply(resi, 1, function(x) {
    x < lcl | x > ucl
  }))
  
  if (plot) { el.plot.est(d, est, alert) }
  
  list(
    fit = list(mtx = mtx,
               alpha = alpha,
               ucl = ucl,
               lcl = lcl),
    score = list(est = est,
                 resi = resi,
                 alert = alert)
  )
} 

#' Compute scores given multivariate regression model (linear)
#'
#' @param data  matrix or data.frame.
#' @param fit   list(mtx, alpha, ucl. lcl). mvrVd model
#' @param plot  logical. Plot or not
#'
#' @return list(est, resi, alert)
#' @export
#'
#' @examples 
#' tr <- bearing[1:12000, -1]
#' ob <- bearing[-(1:12000), -1]
#' m <- el.mvrVd(tr, alpha = 0.01)
#' s <- el.mvrVdScore(ob, m$fit)
#' s2 <- el.poissonFilter(s$alert)
#' el.plot.est(ob, s$est, s2$score, rows = 4)
#' 
el.mvrVdScore <- function(data, fit, plot = TRUE) {
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.matrix(data)
  
  if (ncol(d) != length(fit$ucl)) {
    logger.error("Number of columms in data is different from model")
    return()
  }
  
  est = d %*% fit$mtx
  resi = d - est
  alert = t(apply(resi, 1, function(x){ x < fit$lcl | x > fit$ucl }))
  
  if (plot) { el.plot.est(d, est, alert) }
  
  list(est = est,
       resi = resi,
       alert = alert)
}


#' From raw data, build a function that measures distance between vectors
#'
#' @param data  matrix or data.frame.
#'
#' @return function(v1, v2) => numeric
#'
#' @examples
#' tr <- bearing[1:12000, -1]
#' f <- el.mvrVd.distanceMeasure(tr)
#' 
el.mvrVd.distanceMeasure <- function(data){
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.matrix(data[complete.cases(data),])
  
  if (nrow(d) < ncol(d) * 4) {
    logger.error("Non-NA data is too small")
    return()
  }
  
  meds <- apply(d, 2, median)
  hs <- sqrt((meds - apply(d, 2, min)) * (apply(d, 2, max) - meds))
  
  function(v1, v2){
    sapply(1:length(v1), function(i){
      
      dif1 <- v1[i] - meds[i]
      if (dif1 == 0)
        at1 <- 0
      else
        at1 <- atan(dif1 / hs[i])
      
      dif2 <- v2[i] - meds[i]
      if (dif2 == 0)
        at2 <- 0
      else
        at2 <- atan(dif2 / hs[i])
      
      theta <- abs(at1 - at2)
      
      1 - theta * pi / 180 / 90
    }
  }
}
