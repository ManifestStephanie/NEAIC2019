#percent loss - stack vs. tmax subframe - total integration time unimportant
sky <- 30*60
obj <- 30.
read <- 1.8
dn <- .008*60
k <- 5*60
fwc <- 10000
max_t <- fwc/(3*sky)
time <- seq(.1, max_t , max_t/100)
xi <- (sky+obj+dn)
p <- 1-(time/max_t*sqrt(max_t/time*(xi*max_t+read^2)/(xi*time+read^2)))
plot(time, p)
signal <- xi*time
mat <- cbind(time,p,signal)
mat
#diffs <- diff(mat[,"p"])/mat[1:length(mat[,"p"])-1,"p"]
#plot(time[1:length(time)-1], diffs)