#' Measure performance of Kmeans culstering with varying k's
#'
#' @param data  vector, matrix, or dta.frame
#' @param plot  plot or not
#'
#' @return vector sum of intra-cluster squared distance
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
    log.error('Too small non-NA data')
    return()
  }
  
  ss <- sapply(1:20, function(k){kmeans(d, centers=k)$tot.withinss})
  
  if(plot) 
    plot(1:20, ss, xlab='k', ylab='Sq. sum of intra-cluster distances', type='l')
  
  ss
}


#' K-Means clustering model & score
#'
#' @param data  vector, matrix, or dta.frame
#' @param plot  plot or not
#'
#' @return list(model=list(k, centers), score)
#' @export
#'
#' @examples
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
    log.error('Too small non-NA data')
    return()
  }
  
  cl <- kmeans(d, centers=k)
  
  if(plot){
    # TODO pca2d plot
    # TODO pca3d plot
  }
  
  list(model=list(k=k, centers=cl$centers), score=as.vector(cl$cluster))
}


#' K-Means cluster membership check
#'
#' @param data  vector, matrix, or dta.frame
#'
#' @param data  
#' @param model k-means clustering model
#'
#' @return vector k-means cluster membership numbers
#' @export
#'
#' @examples el.kmeansScore(iris[,1:4], model)
#' 
el.kmeansScore <- function(data, model){
  
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
    
    which.min(sapply(1:nrow(model$centers), function(i){
      dist(rbind(x, model$centers[i,]))
    }))
  })
  
  as.vector(res)
}