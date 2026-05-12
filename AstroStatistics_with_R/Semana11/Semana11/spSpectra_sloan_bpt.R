espectro1=read.table("/home/torrespapaqui/CursoR/Semana09/spSpec-51612-0280-186.dat", h=T)
attach(espectro1)

plot(espectro1, type="l")
abline(h=1,col='red')

plot(espectro1, type="l", xlim=c(4800,5100))
abline(h=1,col='red')

esp1=subset(espectro1,espectro1$Wave>4800 & espectro1$Wave<=5100)
y1=esp1$Flux
x1=esp1$Wave

plot(x1,y1, type="l")

nonlin_mod3=nls(y1 ~ a*exp(-(x1-m)**2/(2*s**2))+a2*exp(-(x1-m2)**2/(2*s2**2))+a3*exp(-(x1-m3)**2/(2*s3**2)),start=list(a=20,m=4861,s=10,a2=22,m2=4959,s2=6,a3=50,m3=5007,s3=25))

plot(x1,y1, type="l")
lines(x1,predict(nonlin_mod3),col="blue",lwd=3)

nonlin_mod3

plot(x1,y1-predict(nonlin_mod3), type="l")
abline(h=0,col='red',lwd=3)
abline(h=mean(y1-predict(nonlin_mod3)),col='blue',lwd=3)
abline(h=sd(y1-predict(nonlin_mod3)),col='green',lwd=3)
abline(h=-sd(y1-predict(nonlin_mod3)),col='green',lwd=3)

summary(nonlin_mod3)

y11 = 19.63594*exp(-(x1-4861.08155)**2/(2*(-1.63167)**2))
y12 = 20.11281*exp(-(x1-4958.25653)**2/(2*2.32694**2))
y13 = 65.49300*exp(-(x1-5006.16812)**2/(2*2.10338**2))

plot(x1,y1, type="l")
lines(x1,y11, col='red',lwd=2)
lines(x1,y12, col='blue',lwd=2)
lines(x1,y13, col='green',lwd=2)

flux_hb = sum(y11)
flux_o1 = sum(y12)
flux_o2 = sum(y13)

plot(espectro1, type="l", xlim=c(6450,6700))
abline(h=1,col='red')

esp2=subset(espectro1,espectro1$Wave>6450 & espectro1$Wave<=6700)
y2=esp2$Flux
x2=esp2$Wave

plot(x2,y2, type="l")

nonlin_mod6=nls(y2 ~ a4*exp(-(x2-m4)**2/(2*s4**2))+a5*exp(-(x2-m5)**2/(2*s5**2))+a6*exp(-(x2-m6)**2/(2*s6**2)),start=list(a4=20,m4=6548,s4=10,a5=80,m5=6563,s5=10,a6=40,m6=6584,s6=10))

plot(x2,y2, type="l")
lines(x2,predict(nonlin_mod6),col="blue",lwd=3)

plot(x2,y2-predict(nonlin_mod6), type="l")
abline(h=0,col='red',lwd=3)
abline(h=mean(y2-predict(nonlin_mod6)),col='blue',lwd=3)
abline(h=sd(y2-predict(nonlin_mod6)),col='green',lwd=3)
abline(h=-sd(y2-predict(nonlin_mod6)),col='green',lwd=3)

summary(nonlin_mod6)

y21 = 1.731e+01*exp(-(x2-6.548e+03)**2/(2*3.031e+00**2))
y22 = 8.740e+01*exp(-(x2-6.563e+03)**2/(2*2.618e+00**2))
y23 = 5.121e+01*exp(-(x2-6.583e+03)**2/(2*2.803e+00**2))

plot(x2,y2, type="l")
lines(x2,y21, col='red',lwd=2)
lines(x2,y22, col='blue',lwd=2)
lines(x2,y23, col='green',lwd=2)

flux_n1 = sum(y21)
flux_ha = sum(y22)
flux_n2 = sum(y23)


hbo3 = log10(flux_o2/flux_hb)
han2 = log10(flux_n2/flux_ha)

hbo301 = c(hbo3,hbo3)
han201 = c(han2,han2)

plot(han201,hbo301, type="p", xlim=c(-1.4, 0.8), ylim=c(-1.4, 1.6))

x_kauf = seq(-1.4,0.0,0.01)
y_kauf = 1.3 + 0.61 /(x_kauf - 0.05)

plot(han201,hbo301, type="p", xlim=c(-1.4, 0.8), ylim=c(-1.4, 1.6))
lines(x_kauf,y_kauf,col="blue",lwd=3)

x_kew = seq(-1.4,0.3,0.01)
y_kew = 1.19 + 0.61 /(x_kew - 0.47)

plot(han201,hbo301, type="p", xlim=c(-1.4, 0.8), ylim=c(-1.4, 1.6))
lines(x_kauf,y_kauf,col="blue",lwd=3)
lines(x_kew,y_kew,col="red",lwd=3)

x_topap = seq(-0.13,0.80,0.01)
y_topap = 1.64 * x_topap + 0.38

plot(han201,hbo301, type="p", xlim=c(-1.4, 0.8), ylim=c(-1.4, 1.6))
lines(x_kauf,y_kauf,col="blue",lwd=3)
lines(x_kew,y_kew,col="red",lwd=3)
lines(x_topap,y_topap,col="green",lwd=3)

