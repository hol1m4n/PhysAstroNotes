setwd('/home/torrespapaqui/CursoR/Semana08/')
COMBO_loz=read.table('COMBO17.dat',header=T, fill=T)

dim(COMBO_loz) ; names(COMBO_loz)

color01 = COMBO_loz$BjMAG
color02 = COMBO_loz$S280MAG - COMBO_loz$BjMAG

filter01 = COMBO_loz$Mcz >= 0.7 & COMBO_loz$Mcz <= 0.9 & color02 <= 3.0

color01 = color01[filter01]
color02 = color02[filter01]

plot(color01,color02, pch=20, cex=0.5,
xlab=expression(M[B]~~(mag)), ylab=expression(M[280] - M[B]~~(mag)), main='')


# Two-dimensional kernel-density estimator
library(MASS)
library(sm)


# Standardize variables
Mag_std <- scale(color01)
Color_std <- scale(color02)
COMBO_std <- cbind(Mag_std,Color_std)

# Hierarchical clustering
COMBO_dist <- dist(COMBO_std)
COMBO_hc <- hclust(COMBO_dist, method='complete')
COMBO_coph <- cophenetic(COMBO_hc)

cor(COMBO_dist, COMBO_coph)

# Cutting the tree
plot(COMBO_hc, label=F)

# k clusters 5, 4, y 3
COMBO_hc3a <- rect.hclust(COMBO_hc, k=3, border='red')
COMBO_hc4a <- rect.hclust(COMBO_hc, k=4, border='yellow')
COMBO_hc5a <- rect.hclust(COMBO_hc, k=5, border='green')
COMBO_hc6a <- rect.hclust(COMBO_hc, k=6, border='magenta')
COMBO_hc7a <- rect.hclust(COMBO_hc, k=7, border='blue')

COMBO_hc3b <- cutree(COMBO_hc, k=3)
COMBO_hc4b <- cutree(COMBO_hc, k=4)
COMBO_hc5b <- cutree(COMBO_hc, k=5)
COMBO_hc6b <- cutree(COMBO_hc, k=6)
COMBO_hc7b <- cutree(COMBO_hc, k=7)

plot(color01, color02, pch=(19+COMBO_hc3b), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main='', col=c(COMBO_hc3b))

plot(color01, color02, pch=(19+COMBO_hc4b), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main='', col=c(COMBO_hc4b))

plot(color01, color02, pch=(19+COMBO_hc5b), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main='', col=c(COMBO_hc5b))

plot(color01, color02, pch=(19+COMBO_hc6b), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main='', col=c(COMBO_hc6b))

plot(color01, color02, pch=(19+COMBO_hc7b), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main='', col=c(COMBO_hc7b))

