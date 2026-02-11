library(data.table)

pop2 = read.table("/home/hollman/PhysAstroNotes/AstroStatistics_with_R/Semana02/LMC_distance_Pop2.dat", header = TRUE)

yy = seq(1,12,by=1)



yy01 = seq(pop2$DM[1]-pop2$pm[1],pop2$DM[1]+pop2$pm[1], by =0.01)
yy02 = seq(pop2$DM[2]-pop2$pm[2],pop2$DM[2]+pop2$pm[2], by =0.01)
yy03 = seq(pop2$DM[3]-pop2$pm[3],pop2$DM[3]+pop2$pm[3], by =0.01)
yy04 = seq(pop2$DM[4]-pop2$pm[4],pop2$DM[4]+pop2$pm[4], by =0.01)
yy05 = seq(pop2$DM[5]-pop2$pm[5],pop2$DM[5]+pop2$pm[5], by =0.01)
yy06 = seq(pop2$DM[6]-pop2$pm[6],pop2$DM[6]+pop2$pm[6], by =0.01)
yy07 = seq(pop2$DM[7]-pop2$pm[7],pop2$DM[7]+pop2$pm[7], by =0.01)
yy08 = seq(pop2$DM[8]-pop2$pm[8],pop2$DM[8]+pop2$pm[8], by =0.01)
yy09 = seq(pop2$DM[9]-pop2$pm[9],pop2$DM[9]+pop2$pm[9], by =0.01)
yy10 = seq(pop2$DM[10]-pop2$pm[10],pop2$DM[10]+pop2$pm[10], by =0.01)
yy11 = seq(pop2$DM[11]-pop2$pm[11],pop2$DM[11]+pop2$pm[11], by =0.01)
yy12 = seq(pop2$DM[12]-pop2$pm[12],pop2$DM[12]+pop2$pm[12], by =0.01)

yyfinal = c(yy01,yy02,yy03,yy04,yy05,yy06,yy07,yy08,yy09,yy10,yy11,yy12)

m1 = mean(yyfinal)
m2 = sd(yyfinal)
mpref = 18.515
m3 = median(yyfinal)
m4 = quantile(yyfinal)
mpsig = 0.085

plot(pop2$DM,yy,xlim=range(c(18.0,19.0)),xlab="DM")
arrows(pop2$DM-pop2$pm, yy, pop2$DM+pop2$pm, yy, length=0.05, angle=90, code=3)
abline(v=m1)
abline(v=mpref,col="blue")
abline(v=m1+m2,lty=2)
abline(v=m1-m2,lty=2)
abline(v=m3,col="red")
abline(v=m4[2],lty=2,col="red")
abline(v=m4[4],lty=2,col="red")
abline(v=mpref+mpsig,lty=2,col="blue")
abline(v=mpref-mpsig,lty=2,col="blue")

weighted.mean(pop2$DM,pop2$pm)
m5 = weighted.mean(pop2$DM,pop2$pm**-2)

plot(pop2$DM,yy,xlim=range(c(18.2,19.0)),xlab="DM")
arrows(pop2$DM-pop2$pm, yy, pop2$DM+pop2$pm, yy, length=0.05, angle=90, code=3)
abline(v=m5)
abline(v=mpref,col="blue")
abline(v=m5+m2,lty=2)
abline(v=m5-m2,lty=2)
abline(v=m3,col="red")
abline(v=m4[2],lty=2,col="red")
abline(v=m4[4],lty=2,col="red")
abline(v=mpref+mpsig,lty=2,col="blue")
abline(v=mpref-mpsig,lty=2,col="blue")

