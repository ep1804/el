library('futile.logger')

#' Formatted print to console
#'
#' @param msg     character. Message or format string
#' @param ...     Optional arguments for formatted string
#'
#' @export
#' 
#' @examples logger.printf("number: %d", 54)
#' 
logger.printf <- function(msg, ...){
  scat(msg, ...)
}

#' Print log when log level is under DEBUG
#'
#' @param msg     character. Message or format string
#' @param ...     Optional arguments for formatted string
#' @param capture logical. Capture print output of given variables or not
#'
#' @export
#' 
#' @examples logger.debug("value: %d", 54)
#' @examples logger.debug("value:", data.frame(c(1, 2), c("A", "BB")), capture=T)
#' 
logger.debug <- function(msg, ..., capture=FALSE){
  
  if(flog.logger()$threshold >= DEBUG ){
    flog.layout(layout.format(paste('~l [~t ~n ', deparse(sys.call(-1)),
                                    '] ~m', sep='')))
    flog.debug(msg, ..., capture=capture)
  }
}

#' Print log when log level is under INFO
#'
#' @param msg     character. Message or format string
#' @param ...     Optional arguments for formatted string
#' @param capture logical. Capture print output of given variables or not
#'
#' @export
#' @examples logger.info("value: %d", 54)
#' @examples logger.info("value:", data.frame(c(1, 2), c("A", "BB")), capture=T)
#' 
logger.info <- function(msg, ..., capture=FALSE){
  
  if(flog.logger()$threshold >= INFO ){
    flog.layout(layout.format(paste('~l [~t ~n ', deparse(sys.call(-1)),
                                    '] ~m', sep='')))
    flog.info(msg, ..., capture=capture)
  }
}


#' Print log when log level is under WARN
#'
#' @param msg     character. Message or format string
#' @param ...     Optional arguments for formatted string
#' @param capture logical. Capture print output of given variables or not
#'
#' @export
#' @examples logger.warn("value: %d", 54)
#' @examples logger.warn("value:", data.frame(c(1, 2), c("A", "BB")), capture=T)
#' 
logger.warn <- function(msg, ..., capture=FALSE){
  if(flog.logger()$threshold >= WARN ){
    flog.layout(layout.format(paste('~l [~t ~n ', deparse(sys.call(-1)),
                                    '] ~m', sep='')))
    flog.warn(msg, ..., capture=capture)
  }
}


#' Print log when log level is under ERROR
#'
#' @param msg     character. Message or format string
#' @param ...     Optional arguments for formatted string
#' @param capture logical. Capture print output of given variables or not
#'
#' @export
#' @examples logger.error("value: %d", 54)
#' @examples logger.error("value:", data.frame(c(1, 2), c("A", "BB")), capture=T)
#' 
logger.error <- function(msg, ..., capture=FALSE){
  if(flog.logger()$threshold >= ERROR ){
    flog.layout(layout.format(paste('~l [~t ~n ', deparse(sys.call(-1)),
                                    '] ~m', sep='')))
    flog.error(msg, ..., capture=capture)
  }
}


#' Logger configuration
#'
#' @param threshold constant. Logger level constant: DEBUG/INFO/WARN/ERROR
#' @param toConsole logical. Print to console or not
#' @param file      character. Log file name. ignored if NULL
#'
#' @export
#' @examples logger.set(DEBUG)
#' @examples logger.set(toConsole=F)
#' @examples logger.set(file=NULL)
#' 
logger.set <- function(threshold=INFO, toConsole=TRUE, file='1.log'){
  flog.threshold(threshold)
  if(toConsole){
    if(is.null(file))
      flog.appender(appender.console())
    else
      flog.appender(appender.tee(file))
  }else{
    if(is.null(file))
      stop("illegal argument to log.set()")
    else
      flog.appender(appender.file(file))
  }
}

# When sourced, logger is configurated with default parameters
logger.set()
