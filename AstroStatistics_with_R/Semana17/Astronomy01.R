# Construct three galaxy redshift datasets, plot using spatstat
shap <- read.table('Shapley_galaxy.dat', header=T, fill=T)

attach(shap)
dim(shap)
summary(shap)


shap.hi <- shap[(R.A. < 205) & (R.A. > 200) & (Dec. > -34) & (Dec. < -29) ,]

shap.lo <- shap[(R.A. < 214) & (R.A. > 209) & (Dec. > -34) & (Dec. < -27) ,]

shap.clus <- shap[(R.A. <204.5) & (R.A. > 200.4) & (Dec. > -32.5) & (Dec. < -31.0) & (Vel > 11000) & (Vel < 18000),]


# Plot in 3-dimensions using rgl, plot and scatterplot3d

#install.packages('rgl')
library(rgl)


rgl.open()

rgl.points(scale(shap[,1]), scale(shap[,2]), scale(shap[,4]))

rgl.bbox()

rgl.snapshot('Shapley.png')

rgl.close()


plot(shap.lo[,1], shap.lo[,2], cex=(scale(shap.lo[,4])+1.5)/2,
xlab='Right Ascension (degrees)', ylab='Declination (degrees)')

#install.packages('scatterplot3d')
library(scatterplot3d)


scatterplot3d(shap.hi[,c(1,2,4)], pch=20, cex.symbols=0.7, type='p', angl=40, zlim=c(0,50000))

# Preparation of nearest neighbor lists for spdep analysis

#install.packages('spdep')
library(spdep)

shap.lo.mat <- as.matrix(shap.lo[,1:2])
nn.shap.lo <- knearneigh(shap.lo.mat, k=1)
str(nn.shap.lo)

nb.shap.lo <- knn2nb(nn.shap.lo)

plot.nb(nb.shap.lo, shap.lo[,1:2])

nb.wt.shap.lo <- nb2listw(nb.shap.lo,style='B')
summary(nb.wt.shap.lo)

# Application of Moran’s I and Geary’s C statistics

moran(shap.lo.mat[,1], nb.wt.shap.lo, n=length(nb.shap.lo),
S0=Szero(nb.wt.shap.lo))
moran.test(shap.lo.mat[,1], nb.wt.shap.lo)
moran.mc(shap.lo.mat[,1], nb.wt.shap.lo, nsim=10000)
geary(shap.lo.mat[,1], nb.wt.shap.lo, n=length(nb.shap.lo),
n1=length(nb.shap.lo)-1, S0=Szero(nb.wt.shap.lo))
geary.test(shap.lo.mat[,1], nb.wt.shap.lo)

# Variogram analysis: gstat

#install.packages('gstat')
library(gstat)

shap.variog <- variogram(Vel~1, locations=~R.A.+Dec., data=shap)
variog.mod1 <- vgm(7e+07, "Gau", 3.0,2e+07)
variog.fit <- fit.variogram(shap.variog, variog.mod1)
variog.fit

plot(shap.variog, model <- variog.fit, col='black', pch=20,
xlab='Distance (degree)', ylab="Semivariance (km/s*km/s)", lwd=2)

# Variogram analysis: geoR

#install.packages('geoR')
library(geoR)

shap1 <- shap[-c(which(duplicated(shap[,1:2]))),]
shap.geo <- as.geodata(shap1, coords.col=1:2, data.col=4)
points.geodata(shap.geo, cex.min=0.2, cex.max=1.0, pt.div='quart', col='gray')
plot.geodata(shap.geo, breaks=30)

shap.vario <- variog(shap.geo, uvec=seq(0, 10, by=0.5))

plot(shap.vario, lwd=2, cex.lab=1.3, cex.axis=1.3, lty=1)

#Toma mucho tiempo de calculo
#shap.GRF1 <- likfit(shap.geo, ini.cov.pars=c(4e7,0.2))

#summary.likGRF(shap.GRF1)

#lines.variomodel(shap.GRF1, lwd=2, lty=2)

# Preparation for spatstat analysis
#install.packages('spatstat')
library(spatstat)

shap.lo.win <- owin(range(shap.lo[,1]), range(shap.lo[,2]))
centroid.owin(shap.lo.win)
area.owin(shap.lo.win)
shap.lo.ppp <- as.ppp(shap.lo[,c(1,2,4)], shap.lo.win)

# planar point pattern
summary(shap.lo.ppp)

plot(density(shap.lo.ppp,0.3), col=topo.colors(20), main='', xlab='R.A.', ylab='Dec.')

plot(shap.lo.ppp, lwd=2, add=T)

# K function for the Shapley low density region
shap.lo.K <- Kest(shap.lo.ppp, correction='isotropic')
shap.lo.K.bias <- Kest(shap.lo.ppp, correction='none')
plot.fv(shap.lo.K, lwd=2, col='black', main='', xlab='r (degrees)', legend=F)
plot.fv(shap.lo.K.bias, add=T, lty=3, lwd=2, col='black', legend=F)

# Draw envelope of 100 simulations of CSR process
shap.lo.K.env <- envelope(shap.lo.ppp, fun=Kest, nsim=100, global=T)
xx <- c(0, shap.lo.K.env$r, rev(shap.lo.K.env$r), 0)
yy <- c(c(0, shap.lo.K.env$lo), rev(c(0,shap.lo.K.env$hi)))
polygon(xx, yy, col='gray')
plot.fv(shap.lo.K, lwd=2, col='black', main='', add=T, legend=F)
plot.fv(shap.lo.K.bias, add=T, lty=3, lwd=2, col='black', legend=F)

# Similar plot for the L* function
shap.lo.L <- Lest(shap.lo.ppp, correction='isotropic')
shap.lo.L.bias <- Lest(shap.lo.ppp, correction='none')
plot(shap.lo.L$r, (shap.lo.L$iso - shap.lo.L$r), lwd=2, col='black',
main='', xlab='r (degrees)', ylab='L*(r)', ty='l', ylim=c(-0.2,0.2))
lines(shap.lo.L$r, (shap.lo.L$theo - shap.lo.L$r), lwd=2, lty=2)
lines(shap.lo.L$r, (shap.lo.L.bias$un - shap.lo.L$r), lwd=2, lty=3)

# Baddeley J function for the Shapley low-density region
plot(Jest(shap.lo.ppp), lwd=2, col='black', cex.lab=1.3, cex.axis=1.3, main='',
xlab='r (degrees)', legend=F)

# Two-point correlation function
shap.lo.pcf <- pcf(shap.lo.ppp)
plot(shap.lo.pcf, xlim=c(0.0,0.2))

plot(log10(shap.lo.pcf$r[2:512]), log10(shap.lo.pcf$trans[2:512]), type='l',
lwd=2, xlab='log r (degrees)', ylab='log pair correlation fn')
lines(c(-1,0), c(0.78+0.48,0.48), lwd=2, lty=2)
lines(c(-2,0), c(0,0), lwd=2, lty=3)

# Compute Dirichlet (Voronoi) tessellation
shap.lo.dir <- dirichlet(shap.lo.ppp)
summary(shap.lo.dir)
plot(shap.lo.dir, main='')
shap.lo.tile <- tiles(shap.lo.dir)
str(shap.lo.tile)

shap.lo.area <- list(lapply(shap.lo.tile, area.owin))
str(shap.lo.area)

# Select small area tiles as clusters
hist(as.numeric(shap.lo.area[[1]]), breaks=30)
shap.lo.clus <- cut(as.numeric(shap.lo.area[[1]]), breaks=c(0,0.06,1))

plot(shap.lo.dir, main='')
points(shap.lo.ppp, pch=20, cex=0.5)
points(shap.lo.ppp[shap.lo.clus=='(0,0.06]'], pch=1,lwd=2)

# Ordinary kriging using the geoR package
shap.vario <- variog(shap.geo, uvec=seq(0, 10, by=0.2))
plot(shap.vario, lwd=2, lty=1)
shap.variofit <- variofit(shap.vario, cov.model='gaussian')
lines(shap.variofit, lty=2)

shap.grid <- pred_grid(c(193,217), c(-38,-29), by=0.3)
KC <- krige.control(obj.model=shap.variofit)
shap.okrig <- krige.conv(shap.geo, loc=shap.grid, krig=KC)

image(shap.okrig, xlim=c(195,203), ylim=c(-38,-27), col = gray.colors(33))
points(shap.geo, cex.min=0.3, cex.max=1.5, add=T)

image(shap.okrig, loc=shap.grid, val=sqrt(shap.okrig$krige.var),
xlim=c(195,203), ylim=c(-38,-27), zlim=c(3800,5000))
points(shap.geo, cex.min=0.3, cex.max=1.5, add=T)
