context("FT")

test_that("Check Fourier transform", {
  set.seed(1)
  
  trajectory <- function(freq, time){
    t <- seq(0, time, 1 / freq)
    res <- 3 * sin(5 * 2 * pi * t) + rnorm(length(t)) # 5 Hz 
    plot(res, type = "l", xlab='Sec', ylab = 'Trajectory')
    res
  }
 
  # In the following examples, note that with any setting of sampling 
  # freqnency and duration, amplitude density is high at 5 Hz.
  
  el.ft(trajectory(100, 1), m.freq = 100, m.intv = 1)
  
  # Higher resolution due to higher frequency of sampling
  el.ft(trajectory(200, 1), m.freq = 200, m.intv = 1)
  
  # Higher resolution due to longer period of sampling
  el.ft(trajectory(100, 2), m.freq = 100, m.intv = 2) 
  
  # Higher resolution due to both higher frequency & longer period of sampling
  el.ft(trajectory(200, 2), m.freq = 200, m.intv = 2)
  
  # Time series longer than standard interval is considered as 
  # multiple time series and averaged. 4 series here:
  el.ft(trajectory(100, 4), m.freq = 100, m.intv = 1)
  
  # 4 series again. with different standard interval
  ft <- el.ft(trajectory(100, 8), m.freq = 100, m.intv = 2)
  
  expect_true(abs(ft$density[10] / 0.1994353 - 1) < 1E-4)
})

# ref http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html

# c.f. 
# 
# trajectory <- function(freq, time){
#   t <- seq(0, time, 1 / freq)
#   res <- 3 * sin(5 * 2 * pi * t) + rnorm(length(t)) # 5 Hz 
#   plot(res, type = "l", xlab='Sec', ylab = 'Trajectory')
#   res
# }
# 
# plot.fft <- function(x, m.freq = 1, m.intv = length(x)) {
#   lim <- m.freq * m.intv
#   strength <- Mod(fft(x[1:lim]))
#   strength <- head(strength, length(strength) / 2)
#   
#   # TODO: why this scaling is necessary?
#   strength[-1] <- 2 * strength[-1]
#   
#   plot(
#     x = 0:(length(strength) - 1) / m.intv,
#     y = strength,
#     t = "h", lwd = 2, main = "",
#     xlab = "Frequency (Hz)",
#     ylab = "Strength"
#   )
# }
# 
# plot.fft(trajectory(100, 1), m.freq = 100, m.intv = 1)
# plot.fft(trajectory(200, 1), m.freq = 200, m.intv = 1)
# plot.fft(trajectory(100, 2), m.freq = 100, m.intv = 2) 
# plot.fft(trajectory(200, 2), m.freq = 200, m.intv = 2)
# plot.fft(trajectory(100, 4), m.freq = 100, m.intv = 1)
# plot.fft(trajectory(100, 8), m.freq = 100, m.intv = 2)