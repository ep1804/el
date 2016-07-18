#' Plot multivariate data with control limit or alert information
#'
#' @param resi  numeric matrix or data.frame. data to be plotted
#' @param ucl   numeric vector, matrix or data.frame. upper control limits,
#'              if not specified, all UCLs are assumed to be 1
#' @param lcl   numeric vector, matrix or data.frame. lower control limits
#'              if not specified, all UCLs are assumed to be -1
#' @param time  POSIXct. if given, x-axis is changed to time  TODO
#' @param mrow  numeric. max rows. applied to graphic parameter mfrow
#'
#' @export
#'
#' @examples
#' 
el.plot.resi <- function(resi, ucl = NULL, lcl = NULL, time = NULL, mrow = 4) {
    
  if(!el.isValid(resi, 'multiple')) return()

  if (is.vector(ucl)) {
    if (length(ucl) != ncol(resi)) {
      logger.error('illegal ucl data')
      stop()
    }
    ucl <- matrix(rep(ucl, nrow(resi)), ncol = ncol(resi), byrow = T)
  }
  
  if (is.vector(lcl)) {
    if (length(lcl) != ncol(resi)) {
      logger.error('illegal lcl data')
      stop()
    }
    lcl <- matrix(rep(lcl, nrow(resi)), ncol = ncol(resi), byrow = T)
  }
  
  if(mrow > ncol(resi))
    mrow <- ncol(resi)
  
  oldPar <- par(no.readonly = T)
  par(mfrow = c(mrow, 1))
  
  for (i in 1:ncol(resi)) {
    re <- resi[,i]
    uc <- ucl[,i]
    lc <- lcl[,i]
    al <- which(re > uc | re < lc)
    
    plot(re, type='n', ylim=range(re, uc, lc), ylab=colnames(resi)[i])
    abline(v = al, col = 'orange')
    lines(uc, type='l', col='green3')
    lines(lc, type='l', col='green3')
    lines(re, type='l')
  }
  
  par(oldPar)
}
