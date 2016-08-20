requireNamespace('rgl')

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

#' Plot 3D data with given color multivariate data with control limit or alert information
#'
#' @param data numeric matrix or data.frame. Data to be plotted
#' @param col  integer vector. Color index
#' @param pal  character vector. Palette
#'
#' @export
#' 
#' @examples el.plot3(bearing[,-1])
#'
el.plot3 <- function(data, col=NULL, pal=NULL){
  
  if(!el.isValid(data, 'multiple')) return()
  
  if(ncol(data) < 3){
    logger.warn("Too small number of columns: %d", ncol(data))
    return()
  }
  
  if(ncol(data) > 3){
    logger.info("Data has more than 3 column. Using only the first 3 of them.")
    data <- data[, 1:3]
  }
  
  palBefore <- palette()
  
  if (is.null(pal)) {
    logger.info("Color index not given. Using rainbow upto purple.")
    pal <- rainbow(min(nrow(data), 250))
    pal <- pal[1:as.integer(length(pal) * 0.8)] # not using reds in the end
    palette(pal)
    plot(rep(1, length(pal)), col = 1:length(pal), type = 'h', 
         ylab = '', ylim = c(0, 1), main = 'Palette')
  } else {
    palette(pal)
  }
  
  if (is.null(col)) {
    logger.info("Color index not given. Using temporal index.")
    col <- sort(rep(1:length(pal),
                    as.integer(nrow(data) / length(pal)) + 1)[1:nrow(data)])
  } else {
    if (is.vector(col)) {
      if (length(col) != nrow(data)) {
        logger.error('illegal col parameter')
        return()
      }
    }
  }
  
  rgl::plot3d(data, col = col)
  
  palette(palBefore) # reset palette
}
