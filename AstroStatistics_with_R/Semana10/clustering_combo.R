setwd('/home/torrespapaqui/CursoR/Semana08/')
COMBO_loz=read.table('COMBO17_lowz.dat',header=T, fill=T)

dim(COMBO_loz) ; names(COMBO_loz)

plot(COMBO_loz, pch=20, cex=0.5, xlim=c(-22,-7), ylim=c(-2,2.5),
xlab=expression(M[B]~~(mag)), ylab=expression(M[280] - M[B]~~(mag)),
main='')


# Two-dimensional kernel-density estimator
library(MASS)
library(sm)

hsj(COMBO_loz$V1)
hsj(COMBO_loz$V2)

COMBO_loz_sm1 <- kde2d(COMBO_loz[,1], COMBO_loz[,2], h=c(0.5,0.1), 
lims = c(-22,-7,-2,2.5), n=500)

image(COMBO_loz_sm1, col=grey(13:0/15), xlab=expression(M[B]~~(mag)),
      ylab=expression(M[280] - M[B]~~(mag)), xlim=c(-22,-7), ylim=c(-2,2.5),
      xaxp=c(-20,-10,2))
      
COMBO_loz_sm2 <- kde2d(COMBO_loz[,1], COMBO_loz[,2], h=c(1.6,0.4), 
lims = c(-22,-7,-2,2.5), n=500)

image(COMBO_loz_sm2, col=grey(13:0/15), xlab=expression(M[B]~~(mag)),
      ylab=expression(M[280] - M[B]~~(mag)), xlim=c(-22,-7), ylim=c(-2,2.5),
      xaxp=c(-20,-10,2))

      
par(mfrow=c(1,2))
plot(COMBO_loz, pch=20, cex=0.5, xlim=c(-22,-7), ylim=c(-2,2.5),
     xlab=expression(M[B]~~(mag)), ylab=expression(M[280] - M[B]~~(mag)),
     main='')
image(COMBO_loz_sm2, col=grey(13:0/15), xlab=expression(M[B]~~(mag)),
      ylab=expression(M[280] - M[B]~~(mag)), xlim=c(-22,-7), ylim=c(-2,2.5),
      xaxp=c(-20,-10,2))
par(mfrow=c(1,1))

# Standardize variables
Mag_std <- scale(COMBO_loz[,1])
Color_std <- scale(COMBO_loz[,2])
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
str(COMBO_hc5a)

COMBO_hc3b <- cutree(COMBO_hc, k=3)
COMBO_hc4b <- cutree(COMBO_hc, k=4)
COMBO_hc5b <- cutree(COMBO_hc, k=5)
COMBO_hc6b <- cutree(COMBO_hc, k=6)
COMBO_hc7b <- cutree(COMBO_hc, k=7)
str(COMBO_hc5b)

plot(COMBO_loz, pch=(19+COMBO_hc3b), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main='', col=c(COMBO_hc3b))

plot(COMBO_loz, pch=(19+COMBO_hc4b), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main='', col=c(COMBO_hc4b))

plot(COMBO_loz, pch=(19+COMBO_hc5b), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main='', col=c(COMBO_hc5b))

plot(COMBO_loz, pch=(19+COMBO_hc6b), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main='', col=c(COMBO_hc6b))

plot(COMBO_loz, pch=(19+COMBO_hc7b), cex=0.7, xlab=expression(M[B]~~(mag)),
     ylab=expression(M[280] - M[B]~~(mag)), main='', col=c(COMBO_hc7b))
