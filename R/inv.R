requireNamespace('matrixcalc')
requireNamespace('MASS')

#' Inverse of matrix generalized
#'
#' @param data matrix or data.frame.
#'
#' @return matrix. Inverse or pseudo inverse of the given matrix
#' @export
#'
#' @examples el.inv(iris[1:4,1:4])
#' 
el.inv <- function(data){
  
  if(! el.isValid(data, 'multiple')) return()
  
  mtx <- as.matrix(data)
  if(matrixcalc::is.singular.matrix(mtx))
    logger.warn("Pseudo inverse used for singular matrix.")
  
  MASS::ginv(mtx)
}