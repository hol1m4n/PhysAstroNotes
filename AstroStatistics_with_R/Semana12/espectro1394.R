espectro1=read.table("QSO_1394.txt", h=T)
attach(espectro1)

plot(espectro1, type="l")
abline(h=1,col='red')

plot(espectro1, type="l", xlim=c(-40,80))
abline(h=1,col='red')

esp1=subset(espectro1,espectro1$SiIV1394>-40 & espectro1$SiIV1394<=80)
y=1-esp1$data
x=esp1$SiIV1394

plot(x,y, type="l")

nonlin_mod1=nls(y ~ a*exp(-(x-m)**2/(2*s**2)),start=list(a=1,m=0,s=10))

plot(x,y, type="l")
lines(x,predict(nonlin_mod1),col="magenta",lwd=3)

nonlin_mod2=nls(y ~ a*exp(-(x-m)**2/(2*s**2))+a2*exp(-(x-m2)**2/(2*s2**2)),start=list(a=1,m=40,s=10,a2=1,m2=0,s2=10))

plot(x,y, type="l")
lines(x,predict(nonlin_mod2),col="red",lwd=3)


nonlin_mod3=nls(y ~ a*exp(-(x-m)**2/(2*s**2))+a2*exp(-(x-m2)**2/(2*s2**2))+a3*exp(-(x-m3)**2/(2*s3**2)),start=list(a=0.5,m=60,s=10,a2=0.3,m2=-4,s2=6,a3=0.5,m3=32,s3=25))

plot(x,y, type="l")
lines(x,predict(nonlin_mod3),col="blue",lwd=3)

plot(x,y, type="l")
lines(x,predict(nonlin_mod1),col="magenta",lwd=3)
lines(x,predict(nonlin_mod2),col="red",lwd=3)
lines(x,predict(nonlin_mod3),col="blue",lwd=3)

nonlin_mod3

plot(x,y-predict(nonlin_mod1), type="l")
abline(h=0,col='red',lwd=3)
abline(h=mean(y-predict(nonlin_mod1)),col='blue',lwd=3)
abline(h=sd(y-predict(nonlin_mod1)),col='green',lwd=3)
abline(h=-sd(y-predict(nonlin_mod1)),col='green',lwd=3)

plot(x,y-predict(nonlin_mod2), type="l")
abline(h=0,col='red',lwd=3)
abline(h=mean(y-predict(nonlin_mod2)),col='blue',lwd=3)
abline(h=sd(y-predict(nonlin_mod2)),col='green',lwd=3)
abline(h=-sd(y-predict(nonlin_mod2)),col='green',lwd=3)

plot(x,y-predict(nonlin_mod3), type="l")
abline(h=0,col='red',lwd=3)
abline(h=mean(y-predict(nonlin_mod3)),col='blue',lwd=3)
abline(h=sd(y-predict(nonlin_mod3)),col='green',lwd=3)
abline(h=-sd(y-predict(nonlin_mod3)),col='green',lwd=3)

par(mfrow=c(3,1))
plot(x,y-predict(nonlin_mod1), type="l")
abline(h=0,col='red',lwd=3)
abline(h=mean(y-predict(nonlin_mod1)),col='blue',lwd=3)
abline(h=sd(y-predict(nonlin_mod1)),col='green',lwd=3)
abline(h=-sd(y-predict(nonlin_mod1)),col='green',lwd=3)

plot(x,y-predict(nonlin_mod2), type="l")
abline(h=0,col='red',lwd=3)
abline(h=mean(y-predict(nonlin_mod2)),col='blue',lwd=3)
abline(h=sd(y-predict(nonlin_mod2)),col='green',lwd=3)
abline(h=-sd(y-predict(nonlin_mod2)),col='green',lwd=3)

plot(x,y-predict(nonlin_mod3), type="l")
abline(h=0,col='red',lwd=3)
abline(h=mean(y-predict(nonlin_mod3)),col='blue',lwd=3)
abline(h=sd(y-predict(nonlin_mod3)),col='green',lwd=3)
abline(h=-sd(y-predict(nonlin_mod3)),col='green',lwd=3)
par(mfrow=c(1,1))

summary(nonlin_mod3)

y1 = 0.09878*exp(-(x-69.50208)**2/(2*9.48551**2))
y2 = 0.19125*exp(-(x-(-2.13888))**2/(2*5.17480**2))
y3 = 0.41501*exp(-(x-13.70914)**2/(2*21.20819**2))

plot(x,y, type="l")
lines(x,y1, col='red',lwd=2)
lines(x,y2, col='blue',lwd=2)
lines(x,y3, col='green',lwd=2)

plot(x,y, type="l",ylim=c(-0.1,0.6))
lines(x,y1, col='red',lwd=2)
lines(x,y2, col='blue',lwd=2)
lines(x,y3, col='green',lwd=2)

