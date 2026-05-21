library(data.table)
asteroid = fread("asteroids.dat")

hist(asteroid$Density)

den = asteroid$Density
errden =  asteroid$ErrorDen
mean(den)
weighted.mean(den,errden)
sd(den)
var(den)

library(moments)
skewness(den)
kurtosis(den) - 3.0

median(den)
quantile(den,0.25)
quantile(den,0.75)

# Add a Normal Curve (Thanks to Peter Dalgaard)
x <- asteroid$Density
h<-hist(x, breaks=10, col="red", xlab="Density g/cm^3",
        main="Histogram with Normal Curve")
xfit<-seq(min(x),max(x),length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue", lwd=2) 

# Kernel Density Plot
d <- density(asteroid$Density) # returns the density data
plot(d) # plots the results

x <- asteroid$Density
h<-hist(x, breaks=10, col="red", xlab="Density g/cm^3",
        main="Histogram with Normal Curve",prob = TRUE,xlim = c(0.5,5))
xfit<-seq(0,5,length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
lines(xfit, yfit, col="blue", lwd=2) 
lines(density(asteroid$Density),lwd=2,col="yellow")

x <- asteroid$Density
h<-hist(x, breaks=20, col="red", xlab="Density g/cm^3",
        main="Histogram with Normal Curve",prob = TRUE,xlim = c(0.5,5))
xfit<-seq(0,5,length=40)
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x))
lines(xfit, yfit, col="blue", lwd=2) 
lines(density(asteroid$Density),lwd=2,col="yellow")

x1 <- asteroid$Density <= 3.5

x2 = asteroid$Density[x1]

h<-hist(x2, breaks=10, col="red", xlab="Density g/cm^3",
        main="Histogram with Normal Curve",prob = TRUE,xlim = c(0.5,4))
xfit<-seq(0,4,length=40)
yfit<-dnorm(xfit,mean=mean(x2),sd=sd(x2))
lines(xfit, yfit, col="blue", lwd=2) 
lines(density(x2),lwd=2,col="yellow")

x3 <- asteroid$Density <= 2.7

x4 = asteroid$Density[x3]

h<-hist(x4, breaks=12, col="red", xlab="Density g/cm^3",
        main="Histogram with Normal Curve",prob = TRUE,xlim = c(0.5,3))
xfit<-seq(0,4,length=40)
yfit<-dnorm(xfit,mean=mean(x4),sd=sd(x4))
lines(xfit, yfit, col="blue", lwd=2) 
lines(density(x4),lwd=2,col="yellow")

x5 <- asteroid$Density <= 2.2

x6 = asteroid$Density[x5]

h<-hist(x6, breaks=8, col="red", xlab="Density g/cm^3",
        main="Histogram with Normal Curve",prob = TRUE,xlim = c(0.5,3))
xfit<-seq(0,4,length=40)
yfit<-dnorm(xfit,mean=mean(x6),sd=sd(x6))
lines(xfit, yfit, col="blue", lwd=2) 
lines(density(x6),lwd=2,col="yellow")

