requireNamespace('rgl')

#' Measure performance of Kmeans culstering with varying k's
#'
#' @param data  vector, matrix, or data.frame.
#' @param plot  logical. plot or not
#'
#' @return vector. Sum of intra-cluster squared distance
#' @export
#'
#' @examples el.kmeansPerf(iris[,1:4])
#' 
el.kmeansPerf <- function(data, plot = TRUE) {
  
  if(is.vector(data)){
    if(!el.isValid(data, 'single')) return()
    d <- data[! is.na(data)]
    d <- as.matrix(d)
  }else{
    if(!el.isValid(data, 'multiple')) return()
    d <- data[complete.cases(data),]
  }
  
  if(nrow(d) < 20){
    logger.error('Too small non-NA data')
    return()
  }
  
  ss <- sapply(1:20, function(k){kmeans(d, centers=k)$tot.withinss})
  
  if(plot) 
    plot(1:20, ss, xlab='k', ylab='Sq. sum of intra-cluster distances', type='l')
  
  ss
}


#' K-Means clustering model & score
#'
#' @param data     vector, matrix, or data.frame.
#' @param k        numeric. Target number of clusters
#' @param plot     logical. Plot or not
#' @param plot3d   logical. Plot or not
#' @param targetSS numeric. Target intra-cluster distance sq. sum.
#'
#' @return list(fit=list(k, center), score)
#' @export
#'
#' @examples el.kmeans(iris[,-5], 3)
#' 
el.kmeans <- function(data, k, plot=TRUE, plot3d=TRUE, targetSS=NULL){
  
  if(is.vector(data)){
    if(!el.isValid(data, 'single')) return()
    d <- data[! is.na(data)]
    d <- as.matrix(d)
  }else{
    if(!el.isValid(data, 'multiple')) return()
    d <- data[complete.cases(data),]
  }
  
  if(nrow(d) < k){
    logger.error('Too small non-NA data')
    return()
  }
  
  if(is.null(targetSS)){
    cl <- kmeans(d, centers=k)
  }else{
    ss <- Inf
    while(ss >= targetSS * 1.05){
      cl <- kmeans(d, centers=k)
      ss <- cl$tot.withinss
    }
  }
  
  if(plot | plot3d){
    points <- el.pca(d, plot = F, plot3d = F)$score
    
    if(plot){
      oldPar <- par(no.readonly = T)
      plot(points[, 1:2], col = cl$cluster)
      par(oldPar)
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
    d <- data[complete.cases(data),]
  }
  
  res <- apply(data, 1, function(x){
    if(any(is.na(x))) return(NA)
    
    which.min(sapply(1:nrow(fit$center), function(i){
      dist(rbind(x, fit$center[i,]))
    }))
  })
  
  as.vector(res)
}