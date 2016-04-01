library('futile.logger')

#' Formatted print to console
#'
#' @param msg     Message or format string
#' @param ...     Optional arguments for formatted string
#' @export
#' @examples
#' log.printf("number: %d", 54)
log.printf <- function(msg, ...){
  scat(msg, ...)
}

#' Print log when log level is under DEBUG
#'
#' @param msg     Message or format string
#' @param ...     Optional arguments for formatted string
#' @param capture Capture print output of given variables or not, FALSE by default
#' @export
#' @examples log.debug("value: %d", 54)
#' @examples log.debug("value:", data.frame(c(1, 2), c("A", "BB")), capture=T)
log.debug <- function(msg, ..., capture=FALSE){
  if(flog.logger()$threshold >= DEBUG ){
    flog.layout(layout.format(paste('~l [~t ~n ', deparse(sys.call(-1)),
                                    '] ~m', sep='')))
    flog.debug(msg, ..., capture=capture)
  }
}

#' Print log when log level is under INFO
#'
#' @param msg     Message or format string
#' @param ...     Optional arguments for formatted string
#' @param capture Capture print output of given variables or not, FALSE by default
#' @export
#' @examples log.info("value: %d", 54)
#' @examples log.info("value:", data.frame(c(1, 2), c("A", "BB")), capture=T)
log.info <- function(msg, ..., capture=FALSE){
  if(flog.logger()$threshold >= INFO ){
    flog.layout(layout.format(paste('~l [~t ~n ', deparse(sys.call(-1)),
                                    '] ~m', sep='')))
    flog.info(msg, ..., capture=capture)
  }
}


#' Print log when log level is under WARN
#'
#' @param msg     Message or format string
#' @param ...     Optional arguments for formatted string
#' @param capture Capture print output of given variables or not, FALSE by default
#' @export
#' @examples log.warn("value: %d", 54)
#' @examples log.warn("value:", data.frame(c(1, 2), c("A", "BB")), capture=T)
log.warn <- function(msg, ..., capture=FALSE){
  if(flog.logger()$threshold >= WARN ){
    flog.layout(layout.format(paste('~l [~t ~n ', deparse(sys.call(-1)),
                                    '] ~m', sep='')))
    flog.warn(msg, ..., capture=capture)
  }
}


#' Print log when log level is under ERROR
#'
#' @param msg     Message or format string
#' @param ...     Optional arguments for formatted string
#' @param capture Capture print output of given variables or not, FALSE by default
#' @export
#' @examples log.error("value: %d", 54)
#' @examples log.error("value:", data.frame(c(1, 2), c("A", "BB")), capture=T)
log.error <- function(msg, ..., capture=FALSE){
  if(flog.logger()$threshold >= ERROR ){
    flog.layout(layout.format(paste('~l [~t ~n ', deparse(sys.call(-1)),
                                    '] ~m', sep='')))
    flog.error(msg, ..., capture=capture)
  }
}


#' Logger configuration
#'
#' @param threshold constant  Logger level constant: DEBUG/INFO/WARN/ERROR
#' @param toConsole logical   Print to console or not
#' @param file      character Log file name. ignored if NULL
#' @export
#' @examples log.set(DEBUG)
#' @examples log.set(toConsole=F)
#' @examples log.set(file=NULL)
log.set <- function(threshold=INFO, toConsole=TRUE, file='2015.log'){
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
log.set()
