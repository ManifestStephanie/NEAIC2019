#Calculate filter center band shift at given AOI

centerband_shift <- function(theta, lambda_0 = 656.3, index = 1.8) {
  delta_lambda <- lambda_0-lambda_0*sqrt(index^2-(sin(theta))^2)/index
  delta_lambda
}

geom_seq <- function(start, end, multiplier) {
  n <- ceiling(log(end/start)/log(multiplier))
  temp <- seq(0, length.out = n +1)
  start*multiplier^temp
}

f_ratio <- geom_seq(1, 10, sqrt(2))
max_theta <- atan(1/(2*f_ratio))
#theta <- seq(0, max_theta, length.out = 10)
shift_ha <- centerband_shift(max_theta)

plot(f_ratio, shift_ha, type = "b",
     main = "Max H-Alpha CB shift vs. F-ratio",
     xlab = "F-ratio",
     ylab = "Delta lambda (shift) [nm]")
abline(2.5,0,lty = 2, col = "red")
abline(6,0, lty = 3, col = "red")
abline(1.5, 0, lty = 4, col = "red")
legend(8,20,c("12nm Filter","5nm Filter","3nm Filter"), lty = c(3,2,4), col = c("red","red","red"))

shift_oiii <- centerband_shift(max_theta, lambda_0 = 500.7)
plot(f_ratio,shift_oiii, type = "b",
     main = "Max OIII CB shift vs. F-ratio",
     xlab = "F-ratio",
     ylab = "Delta lambda (shift) [nm]")
abline(2.5,0,lty = 2, col = "blue")
abline(6, 0, lty = 3, col = "blue")
abline(1.5, 0, lty = 4, col = "blue")
legend(8,15,c("12nm Filter","5nm Filter", "3nm Filter"), lty = c(3,2,4), col = c("blue","blue","blue"))
