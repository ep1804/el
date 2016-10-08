requireNamespace('GeneCycle')

#' Fourier Transform. Compute periodogram given time series values and their 
#' meta information
#'
#' @param x      Time series values
#' @param m.freq Measurement frequency (in Hz)
#' @param f.0    Fundamental frequency, inverse of measurement interval (in Hz)
#' @param plot   Plot periodogram or not
#'
#' @return list(freq, density)
#' @export
#'
#' @examples
#' # 5 Hz signal sampled with given frequency and duration
#' trajectory <- function(freq, duration){
#'   t <- seq(0, duration, 1 / freq)
#'   res <- 3 * sin(5 * 2 * pi * t) + rnorm(length(t)) # 5 for 5 Hz 
#'   plot(res, type = "l", xlab='Sec', ylab = 'Trajectory')
#'   res
#' }
#' 
#' el.ft(trajectory(100, 1), m.freq = 100, f.0 = 1)
#' 
#' # Time series longer than standard interval (1/f.0):
#' # considered as multiple time series and averaged
#' el.ft(trajectory(100, 4), m.freq = 100, f.0 = 1) 
#' 
#' # Higher resolution due to higher frequency of sampling
#' el.ft(trajectory(200, 1), m.freq = 200, f.0 = 1)
#' 
#' # Higher resolution due to smaller fundamental frequency 
#' # (longer standard interfal)
#' el.ft(trajectory(100, 2), m.freq = 100, f.0 = 0.5)
#'
el.ft <- function(x, m.freq = length(x), f.0 = 1, plot = TRUE) {
  
  if (!el.isValid(x, 'single')) return()
  
  len <- as.integer(m.freq / f.0)
  
  if (length(x) < len) {
    logger.warn('Length of x is too small')
    return()
  }
  
  # If measurement time is longer than standard interval, it is considered 
  # multiple samples. Frequency densities are calculated from each sample 
  # and averaged.
  xm <- matrix(x[1:((length(x) %/% len) * len)], nrow = len)
  
  p <- GeneCycle::periodogram(xm)
  harmonics <- 1:(len / 2)
  
  freq <- p$freq[harmonics] * m.freq
  density <- rowMeans(p$spec)[harmonics] / sum(p$spec)
  
  if (plot) {
    graphics::plot(
      x = freq, y = density, lwd = 2, type = "h",
      xlab = "Harmonics (Hz)",
      ylab = "Amplitute Density"
    )
  }
  
  list(freq = freq, density = density)
}