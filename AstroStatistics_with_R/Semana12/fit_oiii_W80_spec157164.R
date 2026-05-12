espectro1=read.table("spec157164.dat")

plot(espectro1$V1, espectro1$V2, type="l")
lines(espectro1$V1,espectro1$V3,col="red")

plot(espectro1$V1, espectro1$V2, type="l", xlim=c(4820,5080))
lines(espectro1$V1,espectro1$V3,col="red")

wave = espectro1$V1
flux = espectro1$V2 - espectro1$V3

plot(wave, flux, type="l", xlim=c(4820,5080))
abline(h=0.0, col="red")

esp1=subset(espectro1,espectro1$V1>4970 & espectro1$V1<=5030)

x = esp1$V1
y = esp1$V2 - esp1$V3

plot(x,y, type="l")

nonlin_mod1=nls(y ~ a*exp(-(x-m)**2/(2*s**2)),start=list(a=1,m=5006,s=10))

plot(x,y, type="l")
lines(x,predict(nonlin_mod1),col="magenta",lwd=3)

nonlin_mod2=nls(y ~ a*exp(-(x-4998.0)**2/(2*s**2))+a2*exp(-(x-m2)**2/(2*s2**2)),start=list(a=2,s=4,a2=2,m2=5005,s2=10))

plot(x,y, type="l")
lines(x,predict(nonlin_mod2),col="red",lwd=3)


nonlin_mod3=nls(y ~ a*exp(-(x-m)**2/(2*s**2))+a2*exp(-(x-m2)**2/(2*s2**2))+a3*exp(-(x-m3)**2/(2*s3**2)),start=list(a=0.5,m=4995,s=2,a2=0.3,m2=5001,s2=4,a3=0.5,m3=5006,s3=6))

plot(x,y, type="l")
lines(x,predict(nonlin_mod3),col="blue",lwd=3)

plot(x,y, type="l")
lines(x,predict(nonlin_mod1),col="magenta",lwd=3)
lines(x,predict(nonlin_mod2),col="red",lwd=3)
lines(x,predict(nonlin_mod3),col="blue",lwd=3)



