requireNamespace('fpc')
requireNamespace('rgl')

#' K-Means clustering model & score
#'
#' @param data     vector, matrix, or data.frame.
#' @param k        numeric. Target number of clusters. if vector, best k selected
#' @param runs     numeric. number of starts(nstart) in kmeans algorithms
#' @param plot     logical. Plot or not
#'
#' @return list(fit=list(k, center), score)
#' @export
#'
#' @examples el.kmeans(iris[,-5])
#' 
el.kmeans <- function(data, k = 2:8, runs = 50, plot = TRUE) {
  
  if (is.vector(data)) {
    if (!el.isValid(data, 'single')) return()
    d <- data[!is.na(data)]
    d <- as.matrix(d)
  } else{
    if (!el.isValid(data, 'multiple'))
      return()
    d <- data[stats::complete.cases(data), ]
  }
  
  if (nrow(d) < max(k)) {
    logger.error('Too small non-NA data')
    return()
  }
  
  fit <- fpc::kmeansruns(d, krange = k, runs = runs)
  
  logger.info('Kmeans clustering: between_SS / total_SS = %.3f %%', 
              fit$betweenss / fit$totss * 100)
  
  if (plot) {
    oldPar <- graphics::par(no.readonly = T)
    
    if (max(k) <= 8) {
      grDevices::palette('default')
    } else {
      grDevices::palette(grDevices::rainbow(max(k) * 1.25))
    }
    
    if(length(k) > 1){
      plot(
        x = k,
        y = fit$crit[-1],
        type = 'l',
        ylab = 'Calinski-Harabasz Index',
        xlab = '# of clusters (K)',
        main = 'K-means Clustering Performace',
        col = 'black'
      )
    }
    
    points <- el.pca(d, plot = F, plot3d = F)$score
    
    rgl::plot3d(points[, 1:3], col = fit$cluster)
    
    graphics::par(oldPar)
  }
  
  list(fit = list(k = fit$bestk, center = fit$centers),
       score = as.vector(fit$cluster))
}


#' K-Means cluster membership check
#'
#' @param data  vector, matrix, or data.frame.
#' @param fit   K-means clustering model
#'
#' @return vector. K-means cluster membership numbers
#' @export
#'
#' @examples el.kmeansScore(iris[,1:4], el.kmeans(iris[,-5], 3)$fit)
#' 
el.kmeansScore <- function(data, fit){
  
  if(is.vector(data)){
    if(!el.isValid(data, 'single')) return()
    d <- data[! is.na(data)]
    d <- as.matrix(d)
  }else{
    if(!el.isValid(data, 'multiple')) return()
    d <- data[stats::complete.cases(data),]
  }
  
  res <- apply(data, 1, function(x){
    if(any(is.na(x))) return(NA)
    
    which.min(sapply(1:nrow(fit$center), function(i){
      stats::dist(rbind(x, fit$center[i,]))
    }))
  })
  
  as.vector(res)
}