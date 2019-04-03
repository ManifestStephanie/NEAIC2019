s_prime <- function(w, b_prime, f_rat, r = 1.6, exp_time = 180, d=.00817, SNR = 3) {
  (1+sqrt(1+4/(SNR^2)*(r^2 + exp_time*(d +w*b_prime/f_rat^2))))*f_rat^2
}

background <- seq(0,10,length.out = 150)

fsq_red <- s_prime(w = 12, b_prime = background, f_rat = 3.65)
ap_130 <- s_prime(w = 5, b_prime = background, f_rat = 5)

mag_diff <- function(s1, s2) {
  -2.5*log10(s1/s2)
  #s2 is assumed to be bigger than s1
}

delta_M <- mag_diff(fsq_red, ap_130)

plot(background,delta_M, type = "l",
     ylim = c(-.1,.8),
     main = "f/3.65 w/12nm vs. f/5 w/5nm",
     xlab = "Background [e-/s]",
     ylab = "Magnitude advantage"
     )
abline(0, 0, lty = 2)

plot(background, fsq_red, type = "l", col = "blue")
lines(background, ap_130, col = "red")