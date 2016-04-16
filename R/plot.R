#' Plot multivariate estimation results
#'
#' @param ob    numeric matrix or data.frame. observtion
#' @param est   numeric matrix or data.frame. estimation
#' @param alert logical matrix or data.frame. alerts
#' @param time  POSIXct. Time : FIXME
#' @param rows  numeric. Rows for graphic parameter mfrow
#'
#' @return unit
#' @export
#'
#' @examples
#' 
el.plot.est <- function(ob, est, alert, time = NULL, rows = 4) {
  
  if(!el.isValid(ob, 'multiple')) return()
  if(!el.isValid(est, 'multiple')) return()
  
  oldPar <- par(no.readonly = T)
  
  par(mfrow = c(rows, 1))
  for (i in 1:ncol(ob)) {
    matplot(
      cbind(ob[, i], est[, i]),
      type = 'l',
      ylab = paste('V', i, sep = ''),
      col = c(1, 3)
    )
    abline(v = (1:nrow(ob))[alert[, i]], col = 2)
  }
  
  par(oldPar)
}