radiodata = read.table("figure6a_Wall.dat",h=TRUE)
attach(radiodata)

#Plot the data with the regression line
plot(omega ~ theta, data=radiodata, xlab="Log Theta (deg)", ylab="Log Omega (deg)", xlim=c(-0.6,0.3), ylim=c(-0.2,0.4))
radio.lm <- lm(omega ~ theta, data=radiodata)
abline(radio.lm,col='red',lwd=4)

#Coefficients and “official” confidence intervals:
coefficients(radio.lm)
confint(radio.lm)

#The residuals don’t look very Gaussian:
plot(radiodata$theta,residuals(radio.lm))
plot(density(residuals(radio.lm)))

resample <- function(x) {
  sample(x,size=length(x),replace=TRUE)
}

#Find CIs for coefficients by resampling cases:
coefs.radio.lm <- function(subset) {
    fit <- lm(omega ~ theta, data=radiodata, subset=subset)
    return(coefficients(fit))
}

radio.lm.sampling.dist <- replicate(1000, coefs.radio.lm(resample(1:nrow(radiodata))))
limits01 <- apply(radio.lm.sampling.dist,1,quantile,c(0.025,0.975))

limits01

plot(density(residuals(radio.lm)))
abline(v=limits01[1],lty=2,col="green",lwd=2)
abline(v=limits01[2],lty=2,col="green",lwd=2)

#Find CIs for coefficients by resampling cases:
coefs.radio.lm <- function(subset) {
  fit <- lm(omega ~ theta, data=radiodata, subset=subset)
  return(fit$coefficients[2])
}

radio.lm.slope.dist <- replicate(10000, coefs.radio.lm(resample(1:nrow(radiodata))))
limits02 <- quantile(radio.lm.slope.dist,c(0.025,0.975))

limits02

plot(density(radio.lm.slope.dist))
abline(v=radio.lm$coefficients[2],lty=2,col="red",lwd=4)
abline(v=mean(radio.lm.slope.dist),lty=2,col="blue",lwd=2)
abline(v=limits02[1],lty=2,col="green",lwd=2)
abline(v=limits02[2],lty=2,col="green",lwd=2)

#Find CIs for coefficients by resampling cases:
coefs.radio.lm <- function(subset) {
  fit <- lm(omega ~ theta, data=radiodata, subset=subset)
  return(fit$coefficients[1])
}

radio.lm.slope.dist <- replicate(10000, coefs.radio.lm(resample(1:nrow(radiodata))))
limits03 <- quantile(radio.lm.slope.dist,c(0.025,0.975))

limits03

plot(density(radio.lm.slope.dist))
abline(v=radio.lm$coefficients[1],lty=2,col="red",lwd=4)
abline(v=mean(radio.lm.slope.dist),lty=2,col="blue",lwd=2)
abline(v=limits03[1],lty=2,col="green",lwd=2)
abline(v=limits03[2],lty=2,col="green",lwd=2)





# Tarea: Reproducir del articulo de Lopez la figura 4 usando bootstrap
# Reproducir de Morales-Vargas la figura 10 usando  bootstrap

