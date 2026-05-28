# Construct a sample of bright nearby Hipparcos stars
hip <- read.table("/home/holman/Downloads/Semana16/HIP1.tsv",header=T, fill=T)

attach(hip) ; dim(hip); summary(hip)

# Plot luminosity distribution of stars and their truncation limits
AbsMag <- Vmag + 5*log10(Plx/1000) + 5
Lum <- 2.512^(4.84 - AbsMag)

plot(density(log10(Lum)),ylim=c(0,1.7), main="",
     xlab="log L (solar, V band)", lwd=2, cex.lab=1.2)
AbsMaglim <- 10.5 + 5*log10(Plx/1000) + 5
Lumlim <- 2.512^(4.84 - AbsMaglim)
lines(density(log10(Lumlim)), lty=2, lwd=2)
text(0.7, 0.5, "Hipparcos sample", pos=4, font=2)
text(-0.5, 1.2, "Truncation limits", pos=4, font=2)

# Compute Lynden-Bell-Woodroofe estimator with 90% confidence bands
#install.packages("DTDA")
library(DTDA)

LBW.hip <- efron.petrosian(log10(Lum), log10(Lumlim), boot=T, B=100, alpha=0.1)
summary(LBW.hip)

plot(LBW.hip$time, LBW.hip$survival, pch=20, cex=0.6, main="",
     xlab="log L (solar, V band)", ylab="Density", cex.lab=1.2)
upper <- LBW.hip$upper.Sob[-(1000:1013)]
lower <- LBW.hip$lower.Sob[-(1000:1013)]
lines(LBW.hip$time[-(1000:1013)], upper, lty=3)
lines(LBW.hip$time[-(1000:1013)], lower, lty=3)

# Compare with observed Wielen 1983 local star LF
Wielen.MV <- seq(0, 12, 1)
Wielen.LF <- c(35,126,209,380,676,955,1050,891,1120,1410,2140,2510,4470)
points(log10(2.512^(4.84-Wielen.MV)), Wielen.LF/2500., col="red")

# Compare with binned 1/V.max luminosity function (Schmidt 1968)
Vol.max <- (4*pi/3)*(1000/Plx)^3
bin.sum <- function(vol) sum(1/vol)
Lum.bins <- cut(log10(Lum), breaks=seq(-2.5,3.0,0.5), ord=T)
Schmidt.LF <- by(Lum, Lum.bins, bin.sum)
lines(seq(-2.25, 2.75, 0.5), Schmidt.LF/4500, pch=3, lwd=4, col="blue")

