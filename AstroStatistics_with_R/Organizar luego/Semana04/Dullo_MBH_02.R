library(data.table)

dataBH01 = fread("/home/papaqui/CursoR/Semana04/Dullo_MBH_01.dat")
dataBH02 = fread("/home/papaqui/CursoR/Semana04/Dullo_MBH_02.dat")

deltam01 = dataBH01$mFUV - dataBH01$m36
deltam02 = dataBH02$mFUV - dataBH02$m36

plot(deltam01,dataBH01$MBH,ylim=c(4.0,10.0),xlim=c(1.0,8.0))
abline(lm(dataBH01$MBH~deltam01))
points(deltam02,dataBH02$MBH,col="red")
abline(lm(dataBH02$MBH~deltam02),col="red")

