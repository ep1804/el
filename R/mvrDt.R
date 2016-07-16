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
#' model <- el.mvrDt(tr, alpha = 0.05)
#' score <- el.mvrDtScore(ob, model$fit)
#' 
el.mvrDt <- function(data, alpha = 0.05, plot = TRUE) {
  
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.data.frame(data[complete.cases(data),])
  
  if (nrow(d) < ncol(d)) {
    logger.error("Non-NA data is too small")
    return()
  }
  
  trees <- lapply(1:ncol(d), function(i){
    rpart::rpart(as.formula(paste(colnames(d)[i], '~ .')), data = d)
  })
  
  est <- as.data.frame(sapply(1:ncol(d), function(i){
    predict(trees[[i]], d)
  }))
  
  resi <- d - est
  
  ucl = apply(resi, 2, function(x) {
    el.limit(x, alpha = alpha / 2)
  })
  
  lcl = apply(resi, 2, function(x) {
    el.limit(x, alpha = alpha / 2, upper = F)
  })
  
  if (plot) { el.plot.resi(resi, ucl, lcl) }
  
  list( 
    fit = list(trees = trees,
               alpha = alpha,
               ucl = ucl,
               lcl = lcl),
    score = resi
  )
} 

#' Compute scores given multivariate regression model with decision tree
#'
#' @param data  matrix or data.frame.
#' @param fit   list(trees, alpha, ucl. lcl). mvrLm model
#' @param plot  logical. Plot or not
#'
#' @return list(est, resi, alert)
#' @export
#'
#' @examples 
#' model <- el.mvrDt(tr, alpha = 0.05)
#' score <- el.mvrDtScore(ob, model$fit)
#' 
el.mvrDtScore <- function(data, fit, plot = TRUE) {
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.data.frame(data)
  
  if (ncol(d) != length(fit$trees)) {
    logger.error("Number of columms in data is different from model")
    return()
  }
  
  est <- as.data.frame(sapply(1:ncol(d), function(i){
    predict(fit$trees[[i]], d)
  }))
  
  resi = d - est
  
  if (plot) { el.plot.resi(resi, fit$ucl, fit$lcl) }
  
  resi
}
