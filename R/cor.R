requireNamespace('corrplot')

#' Correlation matrix
#'
#' @param data  matrix or data.frame
#' @param plot  plot or not
#'
#' @return matrix  correlation mtx.
#' @export
#'
#' @examples el.cor(iris[,1:3])
#' 
el.cor <- function(data, plot=TRUE){
  
  if(! el.isValid(data, 'multiple')) return()
  
  co <- cor(data, use='pairwise.complete.obs')
  if(plot){
    oldPar <- par(no.readonly = T)
    corrplot::corrplot(co, method='shade')
    par(oldPar)
  }
  
  co
}