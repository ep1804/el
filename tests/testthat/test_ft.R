context("FT")

test_that("Check Fourier transform", {
  set.seed(1)
  
  # this function gives 5 Hz signal sampled with given frequency and duration
  trajectory <- function(freq, duration){
    t <- seq(0, duration, 1 / freq)
    res <- 3 * sin(5 * 2 * pi * t) + rnorm(length(t))      # 5 for 5 Hz 
    plot(res, type = "l", xlab='Sec', ylab = 'Trajectory')
    res
  }
 
  el.ft(trajectory(100, 1), m.freq = 100, f.0 = 1)
  
  # Time series longer than standard interval (1/f.0):
  # considered as multiple time series and averaged
  el.ft(trajectory(100, 4), m.freq = 100, f.0 = 1) 
  
  # Higher resolution due to higher frequency of sampling
  el.ft(trajectory(200, 1), m.freq = 200, f.0 = 1)
  
  # Higher resolution due to smaller fundamental freq. (longer std. interval)
  el.ft(trajectory(100, 2), m.freq = 100, f.0 = 0.5)
  
  ft <- el.ft(trajectory(100, 4), m.freq = 100, f.0 = 1) 
  
  expect_true(abs(ft$density[5] / 0.2038839 - 1) < 1E-4)
})

# ref http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html

# c.f.
# 
# plot.fft <- function(x, m.freq = length(x), f.0 = 1) {
#   lim <- m.freq / f.0
#   strength <- Mod(fft(x[1:lim]))
#   strength <- head(strength, length(strength) / 2)
# 
#   # TODO: why this scaling is necessary?
#   strength[-1] <- 2 * strength[-1]
# 
#   plot(
#     x = 0:(length(strength) - 1) * f.0,
#     y = strength,
#     t = "h", lwd = 2, main = "",
#     xlab = "Frequency (Hz)",
#     ylab = "Strength"
#   )
# }
# 
# plot.fft(trajectory(100, 1), m.freq = 100, f.0 = 1)
# plot.fft(trajectory(100, 4), m.freq = 100, f.0 = 1)
# plot.fft(trajectory(200, 1), m.freq = 200, f.0 = 1)
# plot.fft(trajectory(100, 2), m.freq = 100, f.0 = 0.5)
