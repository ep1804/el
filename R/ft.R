requireNamespace('GeneCycle')

#' Fourier Transform. Compute periodogram given time series values and their 
#' meta information
#'
#' @param x      Time series values
#' @param m.freq Measurement frequency (in Hz)
#' @param m.intv Standard interval of measurement (in Seconds)
#' @param plot   Plot periodogram or not
#'
#' @return list(freq, density)
#' @export
#'
#' @examples
#' trajectory <- function(freq, time){
#'   t <- seq(0, time, 1 / freq)
#'   res <- 3 * sin(5 * 2 * pi * t) + rnorm(length(t)) # 5 Hz 
#'   plot(res, type = "l", xlab='Sec', ylab = 'Trajectory')
#'   res
#' }
#' 
#' # In the following examples, note that with any setting of sampling 
#' # freqnency and duration, amplitude density is always high at 5 Hz.
#' 
#' el.ft(trajectory(100, 1), m.freq = 100, m.intv = 1)
#' 
#' # Higher resolution due to higher frequency of sampling
#' el.ft(trajectory(200, 1), m.freq = 200, m.intv = 1)
#' 
#' # Higher resolution due to longer period of sampling
#' el.ft(trajectory(100, 2), m.freq = 100, m.intv = 2) 
#' 
#' # Higher resolution due to both higher frequency & longer period of sampling
#' el.ft(trajectory(200, 2), m.freq = 200, m.intv = 2)
#' 
#' # Time series longer than standard interval is considered as 
#' # multiple time series and averaged. Four series here:
#' el.ft(trajectory(100, 4), m.freq = 100, m.intv = 1)
#' 
#' # Four series again. with different standard interval
#' el.ft(trajectory(100, 8), m.freq = 100, m.intv = 2)
#' 
el.ft <- function(x, m.freq = 1, m.intv = length(x), plot = TRUE) {
  
  if (!el.isValid(x, 'single')) return()
  
  len <- as.integer(m.freq * m.intv)
  
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
  
  freq <- p$freq[harmonics] * len / m.intv
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