setwd('/home/torrespapaqui/CursoR/Semana09/')

data = read.table('spec155161.dat',header=TRUE)
attach(data)
head(data)

plot(l,fo,type='l')

plot(l,fo,type='l', ylim = c(0.0,1.0))

nonlin_mod=nls(fo~a*l^b,start=list(a=-1,b=0.1))

nonlin_mod

plot(l,fo,type='l', ylim = c(0.0,1.0))
lines(l,predict(nonlin_mod),col="red")

plot(l,fo,type='l', ylim = c(0.0,1.0),xlim=c(4600,4900))

lw1 = l[which(l >= 4700 & l <= 4800)]
fw1 = fo[which(l >= 4700 & l <= 4800)]

plot(l,fo,type='l', ylim = c(0.0,1.0),xlim=c(4900,5900))

lw2 = l[which(l >= 5100 & l <= 5700)]
fw2 = fo[which(l >= 5100 & l <= 5700)]

plot(l,fo,type='l', ylim = c(0.0,1.0),xlim=c(5700,6300))

lw3 = l[which(l >= 5900 & l <= 6100)]
fw3 = fo[which(l >= 5900 & l <= 6100)]

plot(l,fo,type='l', ylim = c(0.0,1.0),xlim=c(6100,6600))

lw4 = l[which(l >= 6350 & l <= 6450)]
fw4 = fo[which(l >= 6350 & l <= 6450)]

plot(l,fo,type='l', ylim = c(0.0,1.0),xlim=c(6600,7100))

lw5 = l[which(l >= 6800 & l <= 6900)]
fw5 = fo[which(l >= 6800 & l <= 6900)]

new_fo = c(fw1,fw2,fw3,fw4,fw5)
new_lo = c(lw1,lw2,lw3,lw4,lw5)

plot(l,fo,type='l', ylim=c(0,4))
abline(v=c(4700,4800),col='green',lwd=4)
abline(v=c(5100,5700),col='blue',lwd=4)
abline(v=c(5900,6100),col='red',lwd=4)
abline(v=c(6350,6450),col='yellow',lwd=4)
abline(v=c(6800,6900),col='magenta',lwd=4)

nonlin_mod=nls(new_fo~a*(new_lo^b),start=list(a=-1,b=0.1)) 

nonlin_mod

plot(l,fo,type='l', ylim = c(0.0,1.0))
lines(new_lo,predict(nonlin_mod),col="red")

snw_cont = mean(new_fo)/sd(new_fo)

snw_cont

fit_fo = 3.3676 * (l^(-0.1694))

cont_lin = fo - fit_fo

plot(l,cont_lin,type='l', ylim = c(-0.05,6.0))
abline(h=0.0,col='magenta',lwd=2)

plot(l,cont_lin,type='l', ylim = c(-0.1,0.5))
abline(h=0.0,col='magenta',lwd=2)

