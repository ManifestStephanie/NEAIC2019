#percent difference stack vs. single K-length subframe
sky <- 30*60
obj <- 14.
read <- 1.8
dn <- .008*60
k <- 5*60
fwc <- 10000
max_t <- fwc/(3*sky)
time <- seq(.1, max_t , max_t/100)
xi <- (sky+obj+dn)
p <- sqrt((k*xi+k/time*read^2)/(k*xi+read^2))-1
plot(time, p)
signal <- xi*time
mat <- cbind(time,p,signal)
mat
diffs <- diff(mat[,"p"])/mat[1:length(mat[,"p"])-1,"p"]
plot(time[1:length(time)-1], diffs)