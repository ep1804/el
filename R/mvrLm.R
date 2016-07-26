#' Multivariate regression with linear model
#'
#' @param data  matrix or data.frame.   
#' @param alpha numeric. Critical level
#' @param plot  logical. Plot or not
#'
#' @return list(fit, score).
#' @export
#'
#' @examples
#' model <- el.mvrLm(tr, alpha = 0.05)
#' score <- el.mvrLmScore(ob, model$fit)
#' 
el.mvrLm <- function(data, alpha = 0.05, plot = TRUE) {
  
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.data.frame(data[complete.cases(data),])
  
  if (nrow(d) < ncol(d)) {
    logger.error("Non-NA data is too small")
    return()
  }
  
  lms <- lapply(1:ncol(d), function(i){
    glm(as.formula(paste(colnames(d)[i], '~ .')), data = d)
  })
  
  est <- as.data.frame(sapply(1:ncol(d), function(i){
    predict(lms[[i]], d)
  }))
  
  resi = d - est
  
  ucl = apply(resi, 2, function(x) {
    el.limit(x, alpha = alpha / 2)
  })
  
  lcl = apply(resi, 2, function(x) {
    el.limit(x, alpha = alpha / 2, upper = F)
  })
  
  if (plot) { el.plot.resi(resi, ucl, lcl) }
  
  list( 
    fit = list(lms = lms,
               alpha = alpha,
               ucl = ucl,
               lcl = lcl),
    score = resi
  )
} 

#' Compute scores given multivariate regression model (linear)
#'
#' @param data  matrix or data.frame.
#' @param fit   list(lms, alpha, ucl, lcl). mvrLm model
#' @param plot  logical. Plot or not
#'
#' @return residual to estimation
#' @export
#'
#' @examples 
#' model <- el.mvrLm(tr, alpha = 0.05)
#' score <- el.mvrLmScore(ob, model$fit)
#' 
el.mvrLmScore <- function(data, fit, plot = TRUE) {
  
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.data.frame(data)
  
  if (ncol(d) != length(fit$lms)) {
    logger.error("Number of columms in data is different from model")
    return()
  }
  
  est <- as.data.frame(sapply(1:ncol(d), function(i){
    predict(fit$lms[[i]], d)
  }))
  
  resi = d - est
  
  if (plot) { el.plot.resi(resi, fit$ucl, fit$lcl) }
  
  resi
}