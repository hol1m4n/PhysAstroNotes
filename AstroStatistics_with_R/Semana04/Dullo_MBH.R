library(data.table)
dataBH = fread("/home/torrespapaqui/CursoR/Semana04/Dullo_MBH.dat")

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
