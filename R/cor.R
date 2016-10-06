requireNamespace('corrplot')

#' Correlation matrix
#'
#' @param data  matrix or data.frame.
#' @param plot  logical. Plot or not
#'
#' @return matrix. Correlation mtx.
#' @export
#'
#' @examples el.cor(iris[,1:3])
#' 
el.cor <- function(data, plot=TRUE){
  
  if(! el.isValid(data, 'multiple')) return()
  
  co <- stats::cor(data, use='pairwise.complete.obs')
  if(plot){
    oldPar <- graphics::par(no.readonly = T)
    corrplot::corrplot(co, method='shade')
    graphics::par(oldPar)
  }
  
  co
}