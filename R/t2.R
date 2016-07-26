
#' T2 model and score
#'
#' @param data    vector, matrix, or data.frame.
#' @param cluster list.  kmeans cluster model
#' @param plot    logical. plot or not
#' @param alpha   numeric. critical index
#'
#' @return list(fit=list(cluster, icov, alpha, ucl), score)
#' @export
#'
#' @examples el.t2(iris[,-5], el.kmeans(iris[,-5], 3)$fit)
#' 
el.t2 <- function(data, cluster = NULL, alpha = 0.05, plot = TRUE) {
  
  if (is.vector(data)) {
    if (!el.isValid(data, 'single')) return()
    d <- data[!is.na(data)]
    d <- as.matrix(d)
  } else{
    if (!el.isValid(data, 'multiple')) return()
    d <- data[complete.cases(data), ]
  }
  
  if (nrow(d) < 1) {
    logger.warn("There's no non-NA data.")
    return()
  }
  
  if (is.null(cluster)) {
    cluster <- list(k = 1, center = as.matrix(kmeans(d, 1)$center, nrow = 1))
    clusterScore <- rep(1, nrow(d))
  } else{
    clusterScore <- el.kmeansScore(d, cluster)
  }
  
  icov <- lapply(1:cluster$k, function(i) {
    d1 <- d[clusterScore == i, ]
    if (nrow(d1) <= ncol(d1)) {
      logger.warn("Too small cluster: %d", i)
      NA
    } else{
      el.inv(cov(d1))
    }
  })
  
  fit <- list(cluster = cluster, icov = icov)
  score <- el.t2Score(data, fit, FALSE)
  ucl <- el.limit(score, alpha)
  fit <- c(fit, list(alpha = alpha, ucl = ucl))
  
  if(plot){
    oldPar <- par(no.readonly = T)
    plot(score, ylab='T2 score', type='l')
    abline(h = fit$ucl, col='red')
    par(oldPar)
  }
  
  list(fit = fit, score=score)
}


#' T2 score given T2 model
#'
#' @param data    vector, matrix, or data.frame.
#' @param fit     T2 model 
#' @param plot    logical. plot or not
#'
#' @return vector. T2 scores
#' @export
#'
#' @examples el.t2Score(iris[,-5], el.t2(iris[,-5], el.kmeans(iris[,-5], 3)$fit)$fit)
#' 
el.t2Score <- function(data, fit, plot = TRUE) {
  
  if(is.vector(data)){
    if(!el.isValid(data, 'single')) return()
    d <- as.matrix(data)
  }else{
    if(!el.isValid(data, 'multiple')) return()
    d <- as.matrix(data)
  }
  
  clus <- el.kmeansScore(d, fit$cluster)
  
  res <- sapply(1:nrow(d), function(i){
    
    if(sum(is.na(d[i])) > 0)
      NA
    else{
      cl <- clus[i]
      
      if(is.na(cl) | ! is.matrix(fit$icov[[cl]]))
        NA
      else{
        v <- as.vector(d[i,]) - as.vector(fit$cluster$center[cl,])
        v %*% fit$icov[[cl]] %*% v
      }
    }
  })
  
  if(plot){
    oldPar <- par(no.readonly = T)
    plot(res, ylab='T2 score', type='l')
    abline(h = fit$ucl, col='red')
    par(oldPar)
  }
  
  res
}