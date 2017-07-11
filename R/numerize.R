#' Numerize logical or factor variables in a data.frame
#'
#' @param data       data.frame
#'
#' @return data.frame. Data frame with numerized variables
#' @export
#'
#' @examples
#' dia <- ggplot2::diamonds
#' dia <- dia[sample(1:nrow(dia), 800),]
#' diaNu <- el.numerize(dia)
#'
el.numerize <- function(data) {

  if (!is.data.frame(data)) {
    logger.error("data should be a data.frame.")
    return()
  }

  data <- as.data.frame(data)
  colnames(data) <- sapply(colnames(data), function(s){ gsub(" ", "_", s) })

  res <- data.frame(matrix(nrow = nrow(data)))

  for (cl in colnames(data)) {
    if (is.logical(data[, cl])) {
      add <- as.matrix(as.numeric(data[, cl]))
      colnames(add) <- cl
      res <- cbind(res, add)
    } else if (is.factor(data[, cl])) {
      fm <- paste('~', cl, '- 1')
      add <- stats::model.matrix(stats::as.formula(fm), data = data)
      randomize <- diff(range(add)) / 1000000 * rnorm(nrow(add) * ncol(add))
      randomize <- matrix(randomize, ncol = ncol(add))
      add <- add + randomize

      res <- cbind(res, add)
    } else {
      add <- as.matrix(data[, cl])
      colnames(add) <- cl
      res <- cbind(res, add)
    }
  }

  colnames(res) <- sapply(colnames(res), function(s){ gsub(" ", "_", s) })
  res[, -1]
}
