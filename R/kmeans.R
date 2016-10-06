requireNamespace('rgl')

#' Measure performance of Kmeans culstering with varying k's
#'
#' @param data  vector, matrix, or data.frame.
#' @param rep   numeric. Number of repeated kmeans clustering
#' @param plot  logical. Plot or not
#'
#' @return vector. Sum of intra-cluster squared distance
#' @export
#'
#' @examples el.kmeansPerf(iris[,1:4])
#' 
el.kmeansPerf <- function(data, rep = 10, plot = TRUE) {
  
  if(is.vector(data)){
    if(!el.isValid(data, 'single')) return()
    d <- data[! is.na(data)]
    d <- as.matrix(d)
  }else{
    if(!el.isValid(data, 'multiple')) return()
    d <- data[stats::complete.cases(data),]
  }
  
  if(nrow(d) < 15){
    logger.error('Too small non-NA data')
    return()
  }
  
  ss <- rep(Inf, 15)
  for(i in 1:rep){
    ss1 <- sapply(1:15, function(k){stats::kmeans(d, centers=k)$tot.withinss})
    ss <- apply(rbind(ss, ss1), 2, min)
  }
  
  if(plot) 
    plot(1:15, ss, xlab='k', ylab='Sq. sum of intra-cluster distances', type='l')
  
  ss
}


#' K-Means clustering model & score
#'
#' @param data     vector, matrix, or data.frame.
#' @param k        numeric. Target number of clusters
#' @param plot     logical. Plot or not
#' @param plot3d   logical. Plot or not
#' @param rep      numeric. Number of repeated kmeans clustering
#' @param targetSS numeric. Target intra-cluster distance sq. sum.
#'
#' @return list(fit=list(k, center), score)
#' @export
#'
#' @examples el.kmeans(iris[,-5], 3)
#' 
el.kmeans <- function(data, k, plot=TRUE, plot3d=TRUE, rep=10, targetSS=NULL){
  
  if(is.vector(data)){
    if(!el.isValid(data, 'single')) return()
    d <- data[! is.na(data)]
    d <- as.matrix(d)
  }else{
    if(!el.isValid(data, 'multiple')) return()
    d <- data[stats::complete.cases(data),]
  }
  
  if(nrow(d) < k){
    logger.error('Too small non-NA data')
    return()
  }
  
  if(is.null(targetSS)){
    ss <- Inf
    for(i in 1:rep){
      cl1 <- stats::kmeans(d, centers=k)
      if(cl1$tot.withinss < ss)
        cl <- cl1
    }
  }else{
    ss <- Inf
    while(ss >= targetSS * 1.05){
      cl <- stats::kmeans(d, centers=k)
      ss <- cl$tot.withinss
    }
  }
  
  if(plot | plot3d){
    grDevices::palette('default')
    points <- el.pca(d, plot = F, plot3d = F)$score
    
    if(plot){
      oldPar <- graphics::par(no.readonly = T)
      plot(points[, 1:2], col = cl$cluster)
      graphics::par(oldPar)
    }
    
    if(plot3d){
      rgl::plot3d(points[,1:3], col = cl$cluster)
    }
  }
  
  list(fit = list(k = k, center = cl$centers),
       score = as.vector(cl$cluster))
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