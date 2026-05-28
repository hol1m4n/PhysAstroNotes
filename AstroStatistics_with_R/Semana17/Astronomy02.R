# Spatial points
# A partir de los datos de Abell 85

abell <- read.table('Abell085.dat', header=T, fill=T)

attach(abell)

# Tamaño 
dim(abell)

# Resumen de Abell 85
# De aquí se obtendrán los valores para ascención recta (R.A.),
# declinación (Dec.) y velocidad (Vel)
summary(abell)


abell.hi <- abell[(R.A. < 11.165) & (R.A. > 9.704) & (Dec. > -10.101) & (Dec. < -8.697) ,]

abell.lo <- abell[(R.A. < 11.165) & (R.A. > 9.704) & (Dec. > -10.101) & (Dec. < -8.697) ,]


abell.clus <- abell[(R.A. <11.165) & (R.A. > 9.704) & (Dec. > -10.101) & 
                      (Dec. < -31.0) & (Vel > 13416) & (Vel < 18732),]


# Graficar en 3d usando rgl, plot y scatterplot

install.packages('rgl')

library(rgl) 

# rgl
rgl.open()
rgl.points(scale(abell[,1]), scale(abell[,2]), scale(abell[,3]))
rgl.bbox()
rgl.snapshot('abell.png')
rgl.close()



plot(abell.lo[,1], abell.lo[,2], cex=(scale(abell.lo[,3])+1.5)/2, col = "steelblue", main = "Abell 85",xlab='Ascención Recta (grados)', ylab='Declinación (grados)')


install.packages('scatterplot3d')
library(scatterplot3d)

scatterplot3d(abell.hi[,c(1,2,3)], pch=8, cex.symbols=0.7, type='p', angl=110, zlim=c(0,50000))


# Preparación de lista de vecinos cercanos para análisis spdep

install.packages('spdep')
library(spdep)

abell.lo.mat <- as.matrix(abell.lo[,1:2])
nn.abell.lo <- knearneigh(abell.lo.mat, k=1)
str(nn.abell.lo)

nb.abell.lo <- knn2nb(nn.abell.lo)

plot.nb(nb.abell.lo, abell.lo[,1:2], col = "steelblue")

nb.wt.abell.lo <- nb2listw(nb.abell.lo,style='B')
summary(nb.wt.abell.lo)


# Aplicación de la estadística de Moran I y Geary C 

moran(abell.lo.mat[,1], nb.wt.abell.lo, n=length(nb.abell.lo),
      S0=Szero(nb.wt.abell.lo))
moran.test(abell.lo.mat[,1], nb.wt.abell.lo)
moran.mc(abell.lo.mat[,1], nb.wt.abell.lo, nsim=10000)
geary(abell.lo.mat[,1], nb.wt.abell.lo, n=length(nb.abell.lo),
      n1=length(nb.abell.lo)-1, S0=Szero(nb.wt.abell.lo))
geary.test(abell.lo.mat[,1], nb.wt.abell.lo)


# Análisis de variograma con gstat

install.packages('gstat')
library(gstat)

abell.variog <- variogram(Vel~1, locations=~R.A.+Dec., data=abell)
variog.mod1 <- vgm(7e+07, "Gau", 3.0,2e+07)
variog.fit <- fit.variogram(abell.variog, variog.mod1)
variog.fit

plot(abell.variog, model <- variog.fit, col = "darkgreen", pch=20,
     xlab='Distancia [grados]', ylab="Semivarianza [km/s*km/s]", lwd=2)

# Variogram analysis: geoR

install.packages("geoR")
library(geoR)

abell1 <- abell[-c(which(duplicated(abell[,1:2]))),]
abell.geo <- as.geodata(abell1, coords.col=1:2, data.col = 4)
points.geodata(abell.geo, cex.min=0.2, cex.max=1.0, pt.div='quart', col='purple')
plot.geodata(abell.geo, breaks=30)

abell.vario <- variog(abell.geo, uvec=seq(0, 10, by=0.5))

plot(abell.vario, lwd=2, cex.lab=1.3, cex.axis=1.3, lty=1)


abell.lo.win <- owin(range(abell.lo[,1]), range(abell.lo[,2]))
centroid.owin(abell.lo.win)
area.owin(abell.lo.win)
abell.lo.ppp <- as.ppp(abell.lo[,c(1,2,3)], abell.lo.win)


#Preparation for spatstat analysis
install.packages('spatstat')
library(spatstat)

# planar point pattern
summary(abell.lo.ppp)

plot(density(abell.lo.ppp,0.3), col=heat.colors(20), main='', 
     xlab='Ascención Recta', ylab='Declinación')

plot(abell.lo.ppp, lwd=2, add=T)

# Función k para las regiones de baja densidad de Abell 85
abell.lo.K <- Kest(abell.lo.ppp, correction='isotropic')
abell.lo.K.bias <- Kest(abell.lo.ppp, correction='none')
plot.fv(abell.lo.K, lwd=2, col='black', main="", xlab="r (grados)", legend=F)
plot.fv(abell.lo.K.bias, add=T, lty=3, lwd=2, col='black', legend=F)



# Dibujar envolvente de 100 proceos CSR simultáneos 
abell.lo.K.env <- envelope(abell.lo.ppp, fun=Kest, nsim=100, global=T)
xx <- c(0, abell.lo.K.env$r, rev(abell.lo.K.env$r), 0)
yy <- c(c(0, abell.lo.K.env$lo), rev(c(0,abell.lo.K.env$hi)))
polygon(xx, yy, col='cyan')
plot.fv(abell.lo.K, lwd=2, col='brown', main='', add=T, legend=F)
plot.fv(abell.lo.K.bias, add=T, lty=3, lwd=2, col='blue', legend=F)


# Un plot similar para la función L*
abell.lo.L <- Lest(abell.lo.ppp, correction='isotropic')
abell.lo.L.bias <- Lest(abell.lo.ppp, correction='none')
plot(abell.lo.L$r, (abell.lo.L$iso - abell.lo.L$r), lwd=3, col='orangered',
     main='', xlab='r (grados)', ylab='L*(r)', ty='l', ylim=c(-0.2,0.2))
lines(abell.lo.L$r, (abell.lo.L$theo - abell.lo.L$r), col = "red",lwd=3, lty=2)
lines(abell.lo.L$r, (abell.lo.L.bias$un - abell.lo.L$r), col ="black",lwd=3, lty=3)



# Función Baddeley J para la regiónd e baja densidad de Abell 85
plot(Jest(abell.lo.ppp), lwd=2, col='steelblue', cex.lab=1.3, 
     cex.axis=1.3, main='', xlab='r (grados)', ylab = "J(r)", legend=F)

# Función de correlación de 2 puntos
abell.lo.pcf <- pcf(abell.lo.ppp)
plot(abell.lo.pcf, xlim=c(0.0,0.2), ylab = "log pair correlation fn", xlab = "log r (degrees)")



plot(log10(abell.lo.pcf$r[2:512]), log10(abell.lo.pcf$trans[2:512]), type='l',
     lwd=2, xlab='log r (degrees)', ylab='log pair correlation fn')
lines(c(-1,0), c(0.78+0.48,0.48), lwd=2, lty=2)
lines(c(-2,0), c(0,0), lwd=2, lty=3)

# Computar teselación de Dirichlet (Voronoi)
abell.lo.dir <- dirichlet(abell.lo.ppp)
summary(abell.lo.dir)
plot(abell.lo.dir, main='Diagrama Voronoi')
abell.lo.tile <- tiles(abell.lo.dir)
str(abell.lo.tile)

abell.lo.area <- list(lapply(abell.lo.tile, area.owin))
str(abell.lo.area)
