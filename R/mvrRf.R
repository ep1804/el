requireNamespace('randomForest')

#' Multivariate regression with random forest model
#'
#' @param data  matrix or data.frame.   
#' @param alpha numeric. Critical level
#' @param plot  logical. Plot or not
#' @param ntree numeric. Number of treed for random forest model
#'
#' @return list(fit, score).
#' @export
#'
#' @examples
#' tr <- bearing[1:12000, -1]
#' ob <- bearing[-(1:12000), -1]
#' m <- el.mvrRf(tr, alpha = 0.01)
#' s <- el.mvrRfScore(ob, m$fit)
#' s2 <- el.poissonFilter(s$alert)
#' el.plot.est(ob, s$est, s2$score, rows = 4)
#' 
el.mvrRf <- function(data, alpha = 0.05, ntree = 100, plot = TRUE) {
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.data.frame(data[complete.cases(data),])
  
  if (nrow(d) < ncol(d)) {
    logger.error("Non-NA data is too small")
    return()
  }
  
  forests <- lapply(1:ncol(d), function(i) {
    randomForest::randomForest(as.formula(paste(colnames(d)[i], '~ .')), 
                               data = d, ntree = ntree)
  })
  
  est <- as.data.frame(sapply(1:ncol(d), function(i){
    predict(forests[[i]], d)
  }))
  
  resi <- d - est
  
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
    fit = list(forests = forests,
               alpha = alpha,
               ucl = ucl,
               lcl = lcl),
    score = list(est = est,
                 resi = resi,
                 alert = alert)
  )
} 

#' Compute scores given multivariate regression model with random forest
#'
#' @param data  matrix or data.frame.
#' @param fit   list(forests, alpha, ucl. lcl). mvrLm model
#' @param plot  logical. Plot or not
#'
#' @return list(est, resi, alert)
#' @export
#'
#' @examples 
#' tr <- bearing[1:12000, -1]
#' ob <- bearing[-(1:12000), -1]
#' m <- el.mvrRf(tr, alpha = 0.01)
#' s <- el.mvrRfScore(ob, m$fit)
#' s2 <- el.poissonFilter(s$alert)
#' el.plot.est(ob, s$est, s2$score, rows = 4)
#' 
el.mvrRfScore <- function(data, fit, plot = TRUE) {
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.data.frame(data)
  
  if (ncol(d) != length(fit$ucl)) {
    logger.error("Number of columms in data is different from model")
    return()
  }
  
  est <- as.data.frame(sapply(1:ncol(d), function(i){
    predict(fit$forests[[i]], d)
  }))
  
  resi = d - est
  
  alert = t(apply(resi, 1, function(x) {
    x < fit$lcl | x > fit$ucl
  }))
  
  if (plot) { el.plot.est(d, est, alert) }
  
  list(est = est,
       resi = resi,
       alert = alert)
}