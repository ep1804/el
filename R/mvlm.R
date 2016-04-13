#' Multivariate non-parametric linear regression model
#'
#' @param data  matrix or data.frame.   
#' @param alpha numeric. Critical level
#' @param plot  logical. Plot or not
#'
#' @return list. mvlm fit and score
#' @export
#'
#' @examples el.mvlm(bearing)
#' 
el.mvlm <- function(data, alpha = 0.001, plot = TRUE) {
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.matrix(data[complete.cases(data),])
  
  if (nrow(d) < ncol(d) * 4) {
    logger.error("Non-NA data is too small")
    return()
  }
  
  state = d %*% el.inv(t(d) %*% d) %*% t(d)
  est = state %*% d
  
  resi = d - est
  ucl = apply(resi, 2, function(x) { el.limit(x, alpha=alpha/2) })
  lcl = apply(resi, 2, function(x) { el.limit(x, alpha=alpha/2, upper = F) })
  
  alert = t(apply(resi, 1, function(x){ x < lcl | x > ucl }))
  
  if (plot) {
    oldPar <- par(no.readonly = T)
    par(mfrow = c(3, 1))
    for (i in 1:ncol(d)) {
      matplot(
        cbind(d[, i], est[, i]),
        type = 'l',
        ylab = paste('V', i, sep = ''),
        col = c(1, 3)
      )
      abline(v = (1:nrow(d))[alert[, i]], col = 2)
    }
    par(oldPar)
  }
  
  list(
    fit = list(state = state,
               ucl = ucl,
               lcl = lcl),
    score = list(est = est,
                 resi = resi,
                 alert = alert)
  )
} 

el.mvlmScore <- function(data, fit, plot = TRUE) {
  if(!el.isValid(data, 'multiple')) return()
  
  d <- as.matrix(data)
  
  if (nrow(d) != nrow(fit$state)) {
    logger.error("Number of rows in data is different from model")
    return()
  }
  
  if (ncol(d) != length(fit$ucl)) {
    logger.error("Number of columms in data is different from model")
    return()
  }
  
  est = fit$state %*% d
  resi = d - est
  alert = t(apply(resi, 1, function(x){ x < fit$lcl | x > fit$ucl }))
  
  if(plot){
    oldPar <- par(no.readonly = T)
    par(mfrow = c(3, 1))
    for (i in 1:ncol(d)) {
      matplot(
        cbind(d[, i], est[, i]),
        type = 'l',
        ylab = paste('V', i, sep = ''),
        col = c(1, 3)
      )
      abline(v = (1:nrow(d))[alert[, i]], col = 2)
    }
    par(oldPar)
  }
  
  list(est = est,
       resi = resi,
       alert = alert)
}
