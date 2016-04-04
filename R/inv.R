requireNamespace('matrixcalc')
requireNamespace('MASS')

#' Inverse of matrix generalized
#'
#' @param data matrix or data.frame
#'
#' @return inverse or pseudo inverse of the given matrix
#' @export
#'
#' @examples el.env(iris[,-5])
#' 
el.inv <- function(data){
  
  if(! el.isValid(data, 'multiple')) return()
  
  mtx <- as.matrix(data)
  if(matrixcalc::is.singular.matrix(mtx))
    log.warn("Pseudo inverse used for singular matrix.")
  
  MASS::ginv(mtx)
}