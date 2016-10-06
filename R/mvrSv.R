requireNamespace('e1071')

#' Multivariate regression with SVR model
#'
#' @param data  matrix or data.frame.   
#' @param alpha numeric. Critical level
#' @param tune  logical. Tune parameters or not
#' @param plot  logical. Plot or not
#'
#' @return list(fit, score).
#' @export
#'
#' @examples
#' model <- el.mvrSv(tr, alpha = 0.05)
#' score <- el.mvrSvScore(ob, model$fit)
#' 
el.mvrSv <- function(data, alpha = 0.05, tune = FALSE, plot = TRUE) {
  
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.data.frame(data[stats::complete.cases(data),])
  
  if (nrow(d) < ncol(d)) {
    logger.error("Non-NA data is too small")
    return()
  }
  
  svms <- lapply(1:ncol(d), function(i){
    if(tune){
      tuned <- e1071::tune(
        e1071::svm,
        stats::as.formula(paste(colnames(d)[i], '~ .')),
        data = d,
        ranges = list(epsilon = seq(0.1, 1, 0.2), cost = 2 ^ seq(0, 9, 2))
      )
      tuned$best.model
    }else{
      e1071::svm(stats::as.formula(paste(colnames(d)[i], '~ .')), data = d)
    }
  })
  
  est <- as.data.frame(sapply(1:ncol(d), function(i){
    stats::predict(svms[[i]], d)
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
    fit = list(svms = svms,
               alpha = alpha,
               ucl = ucl,
               lcl = lcl),
    score = resi
  )
} 

#' Compute scores given multivariate regression model (linear)
#'
#' @param data  matrix or data.frame.
#' @param fit   list(svms, alpha, ucl. lcl). mvrSv model
#' @param plot  logical. Plot or not
#'
#' @return residual to estimation
#' @export
#'
#' @examples 
#' model <- el.mvrSv(tr, alpha = 0.05)
#' score <- el.mvrSvScore(ob, model$fit)
#' 
el.mvrSvScore <- function(data, fit, plot = TRUE) {
  
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.data.frame(data)
  
  if (ncol(d) != length(fit$svms)) {
    logger.error("Number of columms in data is different from model")
    return()
  }
  
  est <- as.data.frame(sapply(1:ncol(d), function(i){
    stats::predict(fit$svms[[i]], d)
  }))
  
  resi = d - est
  
  if (plot) { el.plot.resi(resi, fit$ucl, fit$lcl) }
  
  resi
}
