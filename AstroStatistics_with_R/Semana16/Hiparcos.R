#setwd('/home/papaqui/CursoR/Semana16')

# Preparation of circular datasets from Hipparcos proper motion catalog
hip <- read.table("HIP.dat", header=T, fill=T)

dim(hip)

summary(hip)


hist(hip[,8], breaks=50)
hip1<- hip[hip[,8]<5,]

hip2 = na.omit(hip1)

hip3 = hip2


hip3[,6] <- hip2[,6] - median(hip2[,6])

hip3[,7] <- hip2[,7] - median(hip2[,7])

attach(hip3)

dim(hip3)


nstar <- length(hip3[,6])

const=360. / (2*pi)

theta <- numeric(length=nstar)


for (i in 1:nstar) {

if(pmRA[i]>=0 & pmDE[i]>=0) theta[i] = atan(pmRA[i] / pmDE[i]) * const

if(pmDE[i]<0) theta[i] = 180. + atan(pmRA[i] / pmDE[i]) * const

if(pmRA[i]<0 & pmDE[i]>=0) theta[i] = 360. + atan(pmRA[i] / pmDE[i]) * const

}

hist(theta, breaks=20, lwd=2, xlab='Position angle theta (degrees)', main='')


# Proper motion and circular plots

#install.packages('CircStats')

library(CircStats)


circ.summary(theta)

circ.plot(theta, cex=0.3, pch=20, stack=T, bins=50, shrink=2)

theta=theta / const


plot(pmRA, pmDE, pch=20, cex=0.6, xlab='Proper motion R.A. (mas/yr)',
ylab='Proper motion Dec. (mas/yr)', main='')

abline(0, 0, lty=2, lwd=2) ; abline(0, 100000, lty=2, lwd=2)

est.kappa(theta, bias=T)


# Basic statistics, tests for uniformity, and von Mises tests

#install.packages('circular')

library(circular)


circ.summary(theta)

sqrt(circ.disp(theta)[4])


kuiper(theta)

r.test(theta)

rao.spacing(theta)

watson(theta)

watson(theta,dist='vm')


vm.ml(theta,bias=T)

vm_boot <- vm.bootstrap.ci(theta,bias=T)

vm_boot$mu.ci

vm_boot$kappa.ci

