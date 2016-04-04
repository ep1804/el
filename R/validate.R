#' Given vector, matrix, or data.frame, check if it is all numeric
#'
#' @param data  vector, matrix, or data.frame. 
#' @param type  character. 'single' or 'multiple'
#'
#' @return logical
#' 
el.isValid <- function(data, type) {
  switch(type,
    single = {
      if (!is.vector(data))
        logger.error("Not a vector")
      else if (!is.numeric(data))
        logger.error("Not numeric")
      else {
        if (sum(is.na(data)) >= length(data) / 2)
          logger.warn("Too many NA")
        return(TRUE)
      }
    },
    multiple = {
      if (!is.matrix(data) & !is.data.frame(data))
        logger.error("Not a matrix or data.frame")
      else if (!is.numeric(as.matrix(data)))
        logger.error("Not numeric")
      else {
        if (sum(is.na(data)) >= nrow(data) * ncol(data) / 2)
          logger.warn("Too many NA")
        return(TRUE)
      }
    }
  )
  FALSE
}
