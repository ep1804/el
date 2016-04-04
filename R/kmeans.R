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
el.kmeansPerf <- function(data, plot=T){
  
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
#' @param data  vector, matrix, or data.frame.
#' @param plot  logical. Plot or not
#' @param k     numeric. Target number of clusters
#'
#' @return list(fit=list(k, centers), score)
#' @export
#'
#' @examples el.kmeans(iris[,-5], 3)
#' 
el.kmeans <- function(data, k, plot=T){
  
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
  
  cl <- kmeans(d, centers=k)
  
  if(plot){
    # TODO pca2d plot
    # TODO pca3d plot
  }
  
  list(fit=list(k=k, centers=cl$centers), score=as.vector(cl$cluster))
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
    
    which.min(sapply(1:nrow(fit$centers), function(i){
      dist(rbind(x, fit$centers[i,]))
    }))
  })
  
  as.vector(res)
}