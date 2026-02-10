library(data.table)

setwd('~/CursoR/Semana03')

hubble = read.table("HubbleConstants.dat", header = TRUE)

xseq =seq(16,140,by=4)

hist(hubble$Ho,xseq)

den = density(hubble$Ho)

hist(hubble$Ho,xseq, prob=TRUE, border="chartreuse", col="darkgreen")
lines(density(hubble$Ho),lwd=4,col="red")

filter = hubble$Ho >= 65 & hubble$Ho <= 75

newho = hubble$Ho[filter]

d <- density(newho)

hist(newho, breaks=24, prob=TRUE, border="chartreuse", col="darkgreen")
lines(density(newho), col="blue", lwd=4)

library(statip)

mfv(newho)
