requireNamespace('rgl')

#' PCA(Principal component analysis) model and score
#'
#' @param data    matrix or data.frame.
#' @param plot    logical. Plot or not
#' @param plot3d  logical. Plot3d or not
#'
#' @return list(fit = list(loading, vaCusum, center, scale), score)
#' @export
#'
#' @examples el.pca(iris[,-5])
#' 
el.pca <- function(data, plot=TRUE, plot3d=TRUE){
  
  if (!el.isValid(data, 'multiple')) return()
  d <- data[complete.cases(data), ]
  
  if(nrow(d) < ncol(d)){
    logger.warn('Too small non-NA data')
    return()
  }
  
  pca <- princomp(d, cor = TRUE) # using cor. mtx (scale effect)
  
  va <- pca$sdev * pca$sdev
  vaCusum <- cumsum(va / sum(va))
  
  if(plot){
    palBack <- palette()
    palette('default')
    oldPar <- par(no.readonly = T)
    plot(vaCusum, type='l', xlab='PCA components', ylab='Variance Cusum')
    biplot(pca)
    par(oldPar)
    palette(palBack)
  }
  
  if(plot3d){
    rgl::plot3d(pca$scores)
  }
  
  list(
    fit = list(
      loading = unclass(pca$loadings),
      vaCusum = vaCusum,
      center = pca$center,
      scale = pca$scale
    ),
    score = pca$scores
  )
}


#' Given PCA model, transform raw data to PCA components
#'
#' @param data    matrix or data.frame.
#' @param fit     PCA model
#'
#' @return matrix, PCA components
#' @export
#'
#' @examples el.pcaScore(iris[,-5], el.pca(iris[,-5])$fit)
#' 
el.pcaScore <- function(data, fit){
  if (!el.isValid(data, 'multiple')) return()
  
  t(t(fit$loading) %*% ((t(data) - fit$center) / fit$scale))
}


#' Given PCA model, transform PCA components to raw data
#'
#' @param score   matrix or data.frame.
#' @param fit     PCA model
#'
#' @return matrix, Raw data
#' @export
#'
el.pcaUnscore <- function(score, fit){
  if (!el.isValid(score, 'multiple')) return()
  
  t(t(el.inv(fit$loading)) %*% t(score) * fit$scale + fit$center)
}

el.pcaPlot3 <- function(score, preset=NULL, color=NULL){
  rgl::plot3d(pca$scores, col = color)
}