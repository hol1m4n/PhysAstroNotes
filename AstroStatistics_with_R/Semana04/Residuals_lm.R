# the eruption durations
duration = faithful$eruptions
# the waiting interval
waiting = faithful$waiting

plot(duration, waiting, xlab="Eruption duration", ylab="Time waited")
abline(lm(waiting ~ duration))

############################################################
lm_fit = lm(waiting ~ duration)

plot(duration, waiting, xlab="Eruption duration", ylab="Time waited")
abline(lm_fit,col="red")

View(lm_fit)

plot(lm_fit$residuals)
abline(h=0.0,col="red")

lm_mean = mean(lm_fit$residuals)

plot(lm_fit$residuals)
abline(h=0.0,col="red")
abline(h=lm_mean,col="blue")

lm_sd = sd(lm_fit$residuals)

plot(lm_fit$residuals)
abline(h=0.0,col="red")
abline(h=lm_mean,col="blue")
abline(h=lm_mean+lm_sd,col="green")
abline(h=lm_mean-lm_sd,col="green")
abline(h=lm_mean+2*lm_sd,col="green")
abline(h=lm_mean-2*lm_sd,col="green")

hist(lm_fit$residuals)

hist(lm_fit$residuals, probability = TRUE, ylim=c(0.00,0.08))
lines(density(lm_fit$residuals),col="red")

#######################################
library(MASS)

lm01_fit = rlm(waiting ~ duration)

plot(duration, waiting, xlab="Eruption duration", ylab="Time waited")
abline(lm01_fit,col="red")

View(lm01_fit)

plot(lm01_fit$residuals)
abline(h=0.0,col="red")

lm01_mean = mean(lm01_fit$residuals)

plot(lm01_fit$residuals)
abline(h=0.0,col="red")
abline(h=lm01_mean,col="blue")

lm01_sd = sd(lm01_fit$residuals)

plot(lm01_fit$residuals)
abline(h=0.0,col="red")
abline(h=lm01_mean,col="blue")
abline(h=lm01_mean+lm_sd,col="green")
abline(h=lm01_mean-lm_sd,col="green")

hist(lm01_fit$residuals)

hist(lm01_fit$residuals, probability = TRUE, ylim=c(0.00,0.08))
lines(density(lm01_fit$residuals),col="red")

###############################################

plot(density(lm01_fit$residuals))
lines(density(lm_fit$residuals),col="red")

