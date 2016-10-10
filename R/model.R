requireNamespace('caret')
requireNamespace('glmnet')
requireNamespace('ranger')

#' Fit a model with CV-based parameter tuning, and show in-sample performance
#'
#' @param dat 
#' @param plot    logical. Plot or not
#'
#' @return list(fit = list(loading, vaCusum, center, scale), score)
#' @export
#'
#' @examples el.pca(iris[,-5])
#' 
el.model <- function(dat, plot=TRUE){
  
}