library(MASS)

#Data on weights of 144 cats; 
data(cats)
help(cats)
summary(cats)

q95.gaussian <- qnorm(0.95,mean=mean(cats$Bwt),sd=sd(cats$Bwt))

q95.gaussian

hist(cats$Bwt)
abline(v=q95.gaussian,col='red',lwd=4)

#fit Gaussian, find 95th percentile
rcats.gaussian <- function() {
  rnorm(n=nrow(cats),mean=mean(cats$Bwt),sd=sd(cats$Bwt))
}

est.q95.gaussian <- function(x) {
  m <- mean(x)
  s <- sd(x)
  return(qnorm(0.95,mean=m,sd=s))
}

est.q95.gaussian

#Simulate, plot the sampling distribution from the simulations
sampling.dist.gaussian <- replicate(2000, est.q95.gaussian(rcats.gaussian()))

plot(hist(sampling.dist.gaussian,breaks=50))

plot(density(sampling.dist.gaussian))
abline(v=q95.gaussian,lty=2)

hist(sampling.dist.gaussian,breaks=50,prob=TRUE)
lines(density(sampling.dist.gaussian),col="red",lwd=2)
abline(v=q95.gaussian,lty=2,col="blue",lwd=4)

sd(sampling.dist.gaussian)

hist(sampling.dist.gaussian,breaks=50,prob=TRUE)
lines(density(sampling.dist.gaussian),col="red",lwd=2)
abline(v=q95.gaussian,lty=2,col="blue",lwd=4)
abline(v=q95.gaussian+sd(sampling.dist.gaussian),lty=2,col="green",lwd=4)
abline(v=q95.gaussian-sd(sampling.dist.gaussian),lty=2,col="green",lwd=4)

quantile(sampling.dist.gaussian,c(0.025,0.975))

hist(sampling.dist.gaussian,breaks=50,prob=TRUE)
lines(density(sampling.dist.gaussian),col="red",lwd=2)
abline(v=q95.gaussian,lty=2,col="blue",lwd=4)
abline(v=q95.gaussian+sd(sampling.dist.gaussian),lty=2,col="green",lwd=4)
abline(v=q95.gaussian-sd(sampling.dist.gaussian),lty=2,col="green",lwd=4)
abline(v=quantile(sampling.dist.gaussian,c(0.025,0.975)),lty=2,col="darkgreen",lwd=4)

#Find the basic CI
2*q95.gaussian - quantile(sampling.dist.gaussian,c(0.975,0.025))

alpha = 2*q95.gaussian - quantile(sampling.dist.gaussian,c(0.975,0.025))

hist(sampling.dist.gaussian,breaks=50,prob=TRUE)
lines(density(sampling.dist.gaussian),col="red",lwd=2)
abline(v=q95.gaussian,lty=2,col="blue",lwd=4)
abline(v=alpha[1],lty=2,col="green",lwd=4)
abline(v=alpha[2],lty=2,col="green",lwd=4)

#Compare histogram to fitted Gaussian density and to a smooth density estimate
plot(hist(cats$Bwt),freq=FALSE)
curve(dnorm(x,mean=mean(cats$Bwt),sd=sd(cats$Bwt)),add=TRUE,col="purple",lwd=4)
lines(density(cats$Bwt),col='red',lwd=4)

#Resampling, re-estimating, and finding sampling distribution, standard error, bias, CIs
q95.np <- quantile(cats$Bwt,0.95)

q95.np

resample <- function(x) {
  sample(x,size=length(x),replace=TRUE)
}

est.q95.np <- function(x) {
  quantile(x,0.95)
}

sampling.dist.np <- replicate(10000, est.q95.np(resample(cats$Bwt)))

plot(density(sampling.dist.np))
abline(v=q95.np,lty=2)

quantile(sampling.dist.np,c(0.025,0.975))

2*q95.np - quantile(sampling.dist.np,c(0.975,0.025))

alpha = 2*q95.np - quantile(sampling.dist.np,c(0.975,0.025))

plot(density(sampling.dist.np))
abline(v=q95.np,lty=2,col="blue",lwd=4)
abline(v=alpha[1],lty=2,col="green",lwd=4)
abline(v=alpha[2],lty=2,col="green",lwd=4)


#Plot the data with the regression line
plot(Hwt~Bwt, data=cats, xlab="Body weight (kg)", ylab="Heart weight (g)")
cats.lm <- lm(Hwt ~ Bwt, data=cats)
abline(cats.lm, col="red",lwd=4)

#Coefficients and “official” confidence intervals:
coefficients(cats.lm)
confint(cats.lm)

#The residuals don’t look very Gaussian:
plot(cats$Bwt,residuals(cats.lm))

plot(density(residuals(cats.lm)))

#Find CIs for coefficients by resampling cases:
coefs.cats.lm <- function(subset) {
    fit <- lm(Hwt~Bwt,data=cats,subset=subset)
    return(coefficients(fit))
}

cats.lm.sampling.dist <- replicate(1000, coefs.cats.lm(resample(1:nrow(cats))))
limits <- apply(cats.lm.sampling.dist,1,quantile,c(0.025,0.975))

limits

plot(density(residuals(cats.lm)))
abline(v=limits[1],lty=2,col="green",lwd=4)
abline(v=limits[2],lty=2,col="green",lwd=4)

