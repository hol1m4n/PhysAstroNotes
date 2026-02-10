library(data.table)

setwd('/home/holman/Downloads/Semana03')

hubble = read.table("HubbleConstants.dat", header = TRUE)

xseq =seq(16,140,by=4)

hist(hubble$Ho,xseq)

den = density(hubble$Ho)

hist(hubble$Ho,xseq,prob=TRUE, border="chartreuse", col="darkgreen")
lines(density(hubble$Ho),lwd=4,col="red")

table(hubble$Type)

x1 = hubble$Ho[hubble$Type == "2A"]
xA1 = hubble$Ho[hubble$Type ==  "A"]
xA2 = hubble$Ho[hubble$Type ==  "AC"]
xA3 = hubble$Ho[hubble$Type ==  "AK"]
xA4 = hubble$Ho[hubble$Type ==  "AS"]
xA5 = hubble$Ho[hubble$Type ==  "AV"]
x2 = c(xA1,xA2,xA3,xA4,xA5)
xB1 = hubble$Ho[hubble$Type ==  "B"]
xB2 = hubble$Ho[hubble$Type ==  "BK"]
xB3 = hubble$Ho[hubble$Type ==  "BS"]
x3 = c(xB1,xB2,xB3)
xC1 = hubble$Ho[hubble$Type ==  "C"]
xC2 = hubble$Ho[hubble$Type ==  "CC"]
xC3 = hubble$Ho[hubble$Type ==  "CI"]
xC4 = hubble$Ho[hubble$Type ==  "CK"]
xC5 = hubble$Ho[hubble$Type ==  "CM"]
x4 = c(xC1,xC2,xC3,xC4,xC5)
xD1 = hubble$Ho[hubble$Type ==  "D"]
xD2 = hubble$Ho[hubble$Type ==  "DK"]
x5 = c(xD1,xD2)
xF1 = hubble$Ho[hubble$Type ==  "F"]
xF2 = hubble$Ho[hubble$Type ==  "FK"]
xF3 = hubble$Ho[hubble$Type ==  "FS"]
x6 = c(xF1,xF2,xF3)
xG1 = hubble$Ho[hubble$Type ==  "G"]
xG2 = hubble$Ho[hubble$Type ==  "GK"]
xG3 = hubble$Ho[hubble$Type ==  "GS"]
xG4 = hubble$Ho[hubble$Type ==  "GV"]
x7 = c(xG1,xG2,xG3,xG4)
xH1 = hubble$Ho[hubble$Type ==  "H"]
xH2 = hubble$Ho[hubble$Type ==  "HK"]
x8 = c(xH1,xH2)
xL1 = hubble$Ho[hubble$Type ==  "L"]
xL2 = hubble$Ho[hubble$Type ==  "LC"]
xL3 = hubble$Ho[hubble$Type ==  "LI"]
x9 = c(xL1,xL2,xL3)
xN1 = hubble$Ho[hubble$Type ==  "N"]
xN2 = hubble$Ho[hubble$Type ==  "NAA"]
xN3 = hubble$Ho[hubble$Type ==  "NS"]
xN4 = hubble$Ho[hubble$Type ==  "NV"]
x10 = c(xN1,xN2,xN3,xN4)
xO1 = hubble$Ho[hubble$Type ==  "O"]
xO2 = hubble$Ho[hubble$Type ==  "OC"]
xO3 = hubble$Ho[hubble$Type ==  "OK"]
xO4 = hubble$Ho[hubble$Type ==  "OM"]
xO5 = hubble$Ho[hubble$Type ==  "OS"]
xO6 = hubble$Ho[hubble$Type ==  "OV"]
x11 = c(xO1,xO2,xO3,xO4,xO5,xO6)
xP1 = hubble$Ho[hubble$Type ==  "P"]
xP2 = hubble$Ho[hubble$Type ==  "PK"]
x12 = c(xP1,xP2)
xR1 = hubble$Ho[hubble$Type ==  "R"]
xR2 = hubble$Ho[hubble$Type ==  "rK"]
x13 = c(xR1,xR2)
xS1 = hubble$Ho[hubble$Type ==  "S"]
xS2 = hubble$Ho[hubble$Type ==  "SC"]
xS3 = hubble$Ho[hubble$Type ==  "SI"]
xS4 = hubble$Ho[hubble$Type ==  "SK"]
xS5 = hubble$Ho[hubble$Type ==  "SS"]
xS6 = hubble$Ho[hubble$Type ==  "SV"]
x14 = c(xS1,xS2,xS3,xS4,xS5,xS6)
xT1 = hubble$Ho[hubble$Type ==  "T"]
xT2 = hubble$Ho[hubble$Type ==  "TI"]
xT3 = hubble$Ho[hubble$Type ==  "TK"]
xT4 = hubble$Ho[hubble$Type ==  "TS"]
x15 = c(xT1,xT2,xT3,xT4)
xZ1 = hubble$Ho[hubble$Type ==  "Z"]
xZ2 = hubble$Ho[hubble$Type ==  "ZC"]
xZ3 = hubble$Ho[hubble$Type ==  "ZI"]
x16 = c(xZ1,xZ2,xZ3)

library(vioplot)


vioplot(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16,
names=c("2","A","B","C","D","F","G","H","L","N","O","P","R","S","T","Z"),
col="gold", rectCol="gray", colMed="red")

hist(hubble$Ho,xseq,prob=TRUE, border="chartreuse", col="darkgreen")
lines(density(hubble$Ho),lwd=4,col="red")

library(statip)

mfv(hubble$Ho)

library(lattice)

par(mfrow=c(4,4))
par(mar=c(2,2,2,2))
plot(density(x1))
rug(jitter(x1))
plot(density(x2))
rug(jitter(x2))
par(mfrow=c(1,1))

# Reproducir la figura del articulo

par(mfrow=c(4,4))
par(mar=c(2,2,2,2))
plot(density(xR1))
rug(jitter(xR1))
plot(density(xS1))
rug(jitter(xS1))
plot(density(xT1))
rug(jitter(xT1))
plot(density(xZ1))
rug(jitter(xZ1))
plot(density(xL1))
rug(jitter(xL1))
plot(density(xC1))
rug(jitter(xC1))
plot(density(xO1))
rug(jitter(xO1))
plot(density(xP1))
rug(jitter(xP1))
plot(density(xD1))
rug(jitter(xD1))
plot(density(xF1))
rug(jitter(xF1))
plot(density(xG1))
rug(jitter(xG1))
plot(density(xH1))
rug(jitter(xH1))
plot(density(x1))
rug(jitter(x1))
plot(density(xA1))
rug(jitter(xA1))
plot(density(xB1))
rug(jitter(xB1))
plot(density(xC1))
rug(jitter(xC1))
par(mfrow=c(1,1))


hubble = read.table("HubbleConstants.dat", header = TRUE)

table(hubble$Year)

d50s = hubble$Ho[(hubble$Year >= 1950) & (hubble$Year < 1960) ]
d60s = hubble$Ho[(hubble$Year >= 1960) & (hubble$Year < 1970) ]
d70s = hubble$Ho[(hubble$Year >= 1970) & (hubble$Year < 1980) ]
d80s = hubble$Ho[(hubble$Year >= 1980) & (hubble$Year < 1990) ]
d90s = hubble$Ho[(hubble$Year >= 1990) & (hubble$Year < 2000) ]
d00s = hubble$Ho[(hubble$Year >= 2000) & (hubble$Year < 2010) ]
d10s = hubble$Ho[(hubble$Year >= 2010) ]

boxplot(d50s,d60s,d70s,d80s,d90s,d00s,d10s,
        names = c("50s","60s","70s","80s","90s","00s","10s"))


# Tarea, reproducir la figura 8 del paper de Symbad y el grafico de los boxplots por anyo
