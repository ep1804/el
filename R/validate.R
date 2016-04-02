#' Given vector, matrix, or data.frame, check if it is all numeric
#'
#' @param data  vector, matrix, or data.frame 
#' @param type  'single' or 'multiple'
#'
#' @return logical
#' 
el.isValid <- function(data, type) {
  switch(type,
    single = {
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
    multiple = {
      if (!is.matrix(data) & !is.data.frame(data))
        log.error("Not a matrix or data.frame")
      else if (!is.numeric(as.matrix(data)))
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
