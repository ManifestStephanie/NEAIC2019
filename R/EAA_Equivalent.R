#EAA imaging equivalence time
#make sure to check that sky counts are high enough to approximate Gaussian
read <- .9
#k <- 5*60
fwc <- 10000
max_t <- fwc/(3*sky)
#subtime in minutes
sub_time <- seq(.25, max_t , max_t/100)
obj <- 15/sub_time
sky <- 30*60
dn <- .008*60
xi <- (sky+obj+dn)
#efficiency factor (yet to be defined)
q <- 1

#time multiplier K/Kmaxt
time_multiplier <- max_t/(sub_time*q^2)*(xi*sub_time+read^2)/(xi*max_t+read^2)
snr_oneframe <- obj*sub_time/sqrt(xi*sub_time+read^2)
rec_frames <- 9/snr_oneframe^2
total_snr <- snr_oneframe*sqrt(rec_frames)
mat <- cbind(sub_time,time_multiplier,obj,snr_oneframe,rec_frames, total_snr)
head(mat)

#plots
plot(sub_time, time_multiplier)
plot(sub_time, obj)
plot(sub_time,snr_oneframe)
plot(sub_time, rec_frames)
#plot(sub_time, total_snr)