#' UCL or LCL by bootstrap method
#'
#' @param data       vector   
#' @param alpha      numeric  critical level
#' @param upper      logical  TRUE for ucl, FALSE for lcl
#' @param bootstraps numeric  number of testing in bootstrap method
#'
#' @return numeric  UCL or LCL
#' @export
#'
#' @examples el.limit(beaver1$temp)
#' 
el.limit <- function(data, alpha=0.05, upper=TRUE, bootstraps=200){
  if(!el.isValid(data, 'single')) return()
  
  samSize <- max(10000, length(data))
  
  if(upper) alpha <- 1 - alpha
  
  boots <- sapply(1:bootstraps, function(x){
    samples <- sample(data, size = samSize, replace = T)
    quantile(samples, alpha, na.rm = T)
  })
  
  mean(boots)
}


#' UCL or LCL assuming distribution of given data is normal
#'
#' @param data       vector   
#' @param alpha      numeric  critical level
#' @param upper      logical  TRUE for ucl, FALSE for lcl
#'
#' @return numeric UCL or LCL
#' @export
#'
#' @examples el.zlimit(beaver1$temp)
#'
el.zlimit <- function(data, alpha=0.05, upper=TRUE){
  
  if (!el.isValid(data, 'single')) return()
  
  if (upper) alpha <- 1 - alpha
  
  mean(data, na.rm = T) + qnorm(alpha) * sd(data, na.rm = T)
}