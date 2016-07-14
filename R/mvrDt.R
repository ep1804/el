requireNamespace('rpart')

#' Multivariate regression with decision tree model
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
#' m <- el.mvrDt(tr, alpha = 0.01)
#' s <- el.mvrDtScore(ob, m$fit)
#' s2 <- el.poissonFilter(s$alert)
#' el.plot.est(ob, s$est, s2$score, rows = 4)
#' 
el.mvrDt <- function(data, alpha = 0.05, plot = TRUE) {
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.data.frame(data[complete.cases(data),])
  
  if (nrow(d) < ncol(d)) {
    logger.error("Non-NA data is too small")
    return()
  }
  
  dtrees <- lapply(1:ncol(d), function(i){
    rpart::rpart(as.formula(paste(colnames(d)[i], '~ .')), data = d)
  })
  
  est <- as.data.frame(sapply(1:ncol(d), function(i){
    predict(dtrees[[i]], d)
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
    fit = list(dtrees = dtrees,
               alpha = alpha,
               ucl = ucl,
               lcl = lcl),
    score = list(est = est,
                 resi = resi,
                 alert = alert)
  )
} 

#' Compute scores given multivariate regression model with decision tree
#'
#' @param data  matrix or data.frame.
#' @param fit   list(dtrees, alpha, ucl. lcl). mvrLm model
#' @param plot  logical. Plot or not
#'
#' @return list(est, resi, alert)
#' @export
#'
#' @examples 
#' tr <- bearing[1:12000, -1]
#' ob <- bearing[-(1:12000), -1]
#' m <- el.mvrDt(tr, alpha = 0.01)
#' s <- el.mvrDtScore(ob, m$fit)
#' s2 <- el.poissonFilter(s$alert)
#' el.plot.est(ob, s$est, s2$score, rows = 4)
#' 
el.mvrDtScore <- function(data, fit, plot = TRUE) {
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.data.frame(data)
  
  if (ncol(d) != length(fit$ucl)) {
    logger.error("Number of columms in data is different from model")
    return()
  }
  
  est <- as.data.frame(sapply(1:ncol(d), function(i){
    predict(fit$dtrees[[i]], d)
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
