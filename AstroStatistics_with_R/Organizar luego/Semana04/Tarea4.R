library(data.table)
dataBH = fread("/home/holman/PhysAstroNotes/AstroStatistics_with_R/Semana04/Dist_incl.dat")

deltam = dataBH$mFUV - dataBH$m36

library(ggplot2)
ggplot(dataBH, aes(x=deltam, y=MBH)) + geom_point()

ggplot(dataBH, aes(x=deltam, y=MBH)) +
  geom_point() +
  geom_smooth(method=lm)

x1 = deltam < 5
x2 = deltam >= 5

plot(deltam[x1],dataBH$MBH[x1],ylim=c(4.0,10.0),xlim=c(1.0,8.0))
abline(lm(dataBH$MBH[x1]~deltam[x1]))

plot(deltam[x2],dataBH$MBH[x2],ylim=c(4.0,10.0),xlim=c(1.0,8.0))
abline(lm(dataBH$MBH[x2]~deltam[x2]))

plot(deltam[x1],dataBH$MBH[x1],ylim=c(4.0,10.0),xlim=c(1.0,8.0))
abline(lm(dataBH$MBH[x1]~deltam[x1]))
points(deltam[x2],dataBH$MBH[x2],col="red")
abline(lm(dataBH$MBH[x2]~deltam[x2]),col="red")

dataBH$Type
table(dataBH$Type)

samplearly = dataBH$Type == "E-S0" | dataBH$Type ==  "E0-1" | dataBH$Type ==  "E1" | dataBH$Type ==  "E1-2" | dataBH$Type ==  "E2" | dataBH$Type ==  "E3" | dataBH$Type ==  "E4" | dataBH$Type ==  "S0" | dataBH$Type ==  "Sa"

samplelate = dataBH$Type == "Sab" | dataBH$Type ==  "SABa" | dataBH$Type ==  "SABb" | dataBH$Type ==  "SABc" | dataBH$Type ==  "SABm" | dataBH$Type ==  "Sb" | dataBH$Type ==  "SB0" | dataBH$Type ==  "SB0-a" | dataBH$Type ==  "SBa" | dataBH$Type ==  "SBab" | dataBH$Type ==  "SBb" | dataBH$Type ==  "SBbc" | dataBH$Type ==  "Sbc" | dataBH$Type ==  "SBc" | dataBH$Type ==  "Sc"

deltamearly = dataBH$mFUV[samplearly] - dataBH$m36[samplearly]

plot(deltamearly,dataBH$MBH[samplearly])

deltamlate = dataBH$mFUV[samplelate] - dataBH$m36[samplelate]

plot(deltamlate,dataBH$MBH[samplelate])

plot(deltamearly,dataBH$MBH[samplearly], ylim=c(4,10), xlim=c(0.0,8.0))
abline(lm(dataBH$MBH[samplearly] ~ deltamearly))

plot(deltamlate,dataBH$MBH[samplelate], ylim=c(4,10), xlim=c(0.0,8.0))
abline(lm(dataBH$MBH[samplelate] ~ deltamlate))

plot(deltamearly,dataBH$MBH[samplearly], ylim=c(4,10), xlim=c(0.0,8.0))
abline(lm(dataBH$MBH[samplearly] ~ deltamearly))
points(deltamlate,dataBH$MBH[samplelate], col="blue")
abline(lm(dataBH$MBH[samplelate] ~ deltamlate),col="blue")

samplearly = dataBH$Type == "E-S0" | dataBH$Type ==  "E0-1" | dataBH$Type ==  "E1" | dataBH$Type ==  "E1-2" | dataBH$Type ==  "E2" | dataBH$Type ==  "E3" | dataBH$Type ==  "E4" | dataBH$Type ==  "S0" | dataBH$Type ==  "Sa" | dataBH$Type == "Sab" | dataBH$Type ==  "SABa"

samplelate =  dataBH$Type ==  "SABb" | dataBH$Type ==  "SABc" | dataBH$Type ==  "SABm" | dataBH$Type ==  "Sb" | dataBH$Type ==  "SB0" | dataBH$Type ==  "SB0-a" | dataBH$Type ==  "SBa" | dataBH$Type ==  "SBab" | dataBH$Type ==  "SBb" | dataBH$Type ==  "SBbc" | dataBH$Type ==  "Sbc" | dataBH$Type ==  "SBc" | dataBH$Type ==  "Sc"

deltamearly = dataBH$mFUV[samplearly] - dataBH$m36[samplearly]
deltamlate = dataBH$mFUV[samplelate] - dataBH$m36[samplelate]

plot(deltamearly,dataBH$MBH[samplearly], ylim=c(4,10), xlim=c(0.0,8.0))
abline(lm(dataBH$MBH[samplearly] ~ deltamearly))
points(deltamlate,dataBH$MBH[samplelate], col="blue")
abline(lm(dataBH$MBH[samplelate] ~ deltamlate),col="blue")

# Tarea 4
# Empezamos desde aqui la reproduccion de las figuras del articulo


dataBH = fread("/home/holman/PhysAstroNotes/AstroStatistics_with_R/Semana04/Dist_incl.dat")

# Set layout to 1 row, 2 columns
par(mfrow = c(1, 2))

# para comparar, vamos a plotear la figura 2.a primero
deltam = dataBH$mFUV - dataBH$m36

x1 = deltam < 5
x2 = deltam >= 5

plot(deltam[x1],dataBH$MBH[x1],ylim=c(4.0,10.0),xlim=c(1.0,8.0), xlab="m_FUV - m_3.6", ylab="M_BH/M_solar")
abline(lm(dataBH$MBH[x1]~deltam[x1]))
points(deltam[x2],dataBH$MBH[x2],col="red")
abline(lm(dataBH$MBH[x2]~deltam[x2]),col="red")

# La figura 2.b
deltam = dataBH$mNUV - dataBH$m36

x1 = deltam < 5
x2 = deltam >= 5

plot(deltam[x1],dataBH$MBH[x1],ylim=c(4.0,10.0),xlim=c(1.0,8.0), xlab="m_NUV - m_3.6", ylab="M_BH/M_solar")
abline(lm(dataBH$MBH[x1]~deltam[x1]))
points(deltam[x2],dataBH$MBH[x2],col="red")
abline(lm(dataBH$MBH[x2]~deltam[x2]),col="red")

dataBH_new = fread("/home/holman/PhysAstroNotes/AstroStatistics_with_R/Semana04/Dist_incl.dat")

par(mfrow = c(1, 1))

plot(dataBH_new$M3.6m, dataBH$MBH, 
     xlim = c(-18,-23), xlab = "M3.6m", ylab = "M_BH")

abline(lm(dataBH_new$MBH~dataBH_new$M3.6m),col="red")
