#' Given vector, matrix, or data.frame, check if it is all numeric
#'
#' @param data  vector, matrix, or data.frame 
#'
#' @return logical
ear.isValid <- function(data, type = c('vector', 'matrix', 'data.frame')) {
  switch(type,
    vector = {
      if (!is.vector(data))
        log.error("Not a vector")
      else if (!is.numeric(data))
        log.error("Not numeric")
      else {
        if (sum(is.na(data)) >= length(data) / 2)
          log.warn("Too many NA")
        return(TRUE)
      }
    },
    matrix = {
      if (!is.matrix(data))
        log.error("Not a matrix")
      else if (!is.numeric(data))
        log.error("Not numeric")
      else {
        if (sum(is.na(data)) >= nrow(data) * ncol(data) / 2)
          log.warn("Too many NA")
        return(TRUE)
      }
    },
    data.frame = {
      if (!is.data.frame(data))
        log.error("Not a data.frame")
      else if (!is.numeric(data))
        log.error("Not numeric")
      else {
        if (sum(is.na(data)) >= nrow(data) * ncol(data) / 2)
          log.warn("Too many NA")
        return(TRUE)
      }
    }
  )
  FALSE
}