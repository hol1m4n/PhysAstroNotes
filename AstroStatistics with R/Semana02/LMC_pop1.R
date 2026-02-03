library(data.table)

pop1 = read.table("/home/torrespapaqui/CursoR/Semana02/LMC_distance_Pop1.dat", header = TRUE)

yy = seq(1,13,by=1)

plot(pop1$DM,yy,xlim=range(c(18.2,19.0)),xlab="DM")
arrows(pop1$DM-pop1$pm, yy, pop1$DM+pop1$pm, yy, length=0.05, angle=90, code=3)

m1 = mean(pop1$DM)
mpref = 18.50

plot(pop1$DM,yy,xlim=range(c(18.2,19.0)),xlab="DM")
arrows(pop1$DM-pop1$pm, yy, pop1$DM+pop1$pm, yy, length=0.05, angle=90, code=3)
abline(v=m1)
abline(v=mpref,col="blue")

m2 = sd(pop1$DM)
mpsig = 0.085

plot(pop1$DM,yy,xlim=range(c(18.2,19.0)),xlab="DM")
arrows(pop1$DM-pop1$pm, yy, pop1$DM+pop1$pm, yy, length=0.05, angle=90, code=3)
abline(v=m1)
abline(v=mpref,col="blue")
abline(v=m1+m2,lty=2)
abline(v=m1-m2,lty=2)
abline(v=mpref+mpsig,lty=2,col="blue")
abline(v=mpref-mpsig,lty=2,col="blue")

hist(pop1$DM)

m3 = median(pop1$DM)
m4 = quantile(pop1$DM)

plot(pop1$DM,yy,xlim=range(c(18.2,19.0)),xlab="DM")
arrows(pop1$DM-pop1$pm, yy, pop1$DM+pop1$pm, yy, length=0.05, angle=90, code=3)
abline(v=m3,col="red")
abline(v=m4[2],lty=2,col="red")
abline(v=m4[4],lty=2,col="red")

plot(pop1$DM,yy,xlim=range(c(18.2,19.0)),xlab="DM")
arrows(pop1$DM-pop1$pm, yy, pop1$DM+pop1$pm, yy, length=0.05, angle=90, code=3)
abline(v=m1)
abline(v=mpref,col="blue")
abline(v=m1+m2,lty=2)
abline(v=m1-m2,lty=2)
abline(v=m3,col="red")
abline(v=m4[2],lty=2,col="red")
abline(v=m4[4],lty=2,col="red")
abline(v=mpref+mpsig,lty=2,col="blue")
abline(v=mpref-mpsig,lty=2,col="blue")
