# Read dataset on beryllium and lithium abundances in stars
abun <- read.table("/home/holman/Downloads/Semana16/censor_Be.dat", header=T)

dim(abun) ; names(abun) ; attach(abun)

# Boxplot of abundances for stars with and without planets
#install.packages("NADA")
library(NADA)

cen_Be <- seq(FALSE, FALSE, length=68) ; cen_Be[Ind_Be==0] <- TRUE
cen_Li <- seq(FALSE, FALSE, length=68) ; cen_Li[Ind_Li==0] <- TRUE
Type_factor <- as.factor(Type)

par(mfrow=c(1,2))
cenboxplot(logN_Be, cen_Be, Type_factor, log=FALSE, ylab="log N(Be)",
           names=c("Planets","No planets"), boxwex=0.5, notch=TRUE, varwidth=TRUE,
           cex.axis=1.5, cex.lab=1.5, lwd=2)
cenboxplot(logN_Li, cen_Li, Type_factor, log=FALSE, ylab="log N(Li)",
           names=c("Planets","No planets"), boxwex=0.5, notch=TRUE, varwidth=TRUE,
           cex.axis=1.5, cex.lab=1.5, lwd=2)
par(mfrow=c(1,1))

# Test significance of possible lithium abundance effect
logN_Li1 <- logN_Li[-c(1,23)]
cen_Li1 <- cen_Li[-c(1,23)]

Type_factor1 <- Type_factor[-c(1,23)] # remove NaN values

cendiff(logN_Li1, cen_Li1, Type_factor1, rho=0)
cendiff(logN_Li1, cen_Li1, Type_factor1,rho=1)

# Reproduce Santos et al. (2002) plot of stellar beryllium vs. lithium abundance
ind_det1 <- which(Ind_Li==1 & Ind_Be==1 & Type==1) # filled circles
ind_det2 <- which(Ind_Li==1 & Ind_Be==1 & Type==2) # open circles
ind_left1 <- which(Ind_Li==0 & Ind_Be==1 & Type==1)
ind_left2 <- which(Ind_Li==0 & Ind_Be==1 & Type==2)
ind_down1 <- which(Ind_Li==1 & Ind_Be==0 & Type==1)
ind_down2 <- which(Ind_Li==1 & Ind_Be==0 & Type==2)
ind_both1 <- which(Ind_Li==0 & Ind_Be==0 & Type==1)
ind_both2 <- which(Ind_Li==0 & Ind_Be==0 & Type==2)

plot(logN_Li[ind_det1], logN_Be[ind_det1], xlim=c(-0.6,3.5), ylim=c(-0.2,1.5),
     main="", xlab="log N(Li)",
     ylab="log N(Be)", pch=16, lwd=2) # plot detections
points(logN_Li[ind_det2], logN_Be[ind_det2], pch=1)
arrows(logN_Li[ind_left1], logN_Be[ind_left1], logN_Li[ind_left1]-0.2,
       logN_Be[ind_left1],length=0.1) # plot left arrows
arrows(logN_Li[ind_left2], logN_Be[ind_left2], logN_Li[ind_left2]-0.2,
       logN_Be[ind_left2],length=0.1)
points(logN_Li[ind_left1], logN_Be[ind_left1], pch=16)
points(logN_Li[ind_left2], logN_Be[ind_left2], pch=1)
arrows(logN_Li[ind_down1], logN_Be[ind_down1], logN_Li[ind_down1],
       logN_Be[ind_down1]-0.1, length=0.1)
arrows(logN_Li[ind_down2], logN_Be[ind_down2], logN_Li[ind_down2],
       logN_Be[ind_down2]-0.1, length=0.1)
points(logN_Li[ind_down1], logN_Be[ind_down1], pch=16)
points(logN_Li[ind_down2], logN_Be[ind_down2], pch=1)

arrows(logN_Li[ind_both1], logN_Be[ind_both1],
       logN_Li[ind_both1]-0.2, logN_Be[ind_both1], length=0.1) # plot double arrows
arrows(logN_Li[ind_both1], logN_Be[ind_both1], logN_Li[ind_both1],
       logN_Be[ind_both1]-0.1,length=0.1)
arrows(logN_Li[ind_both2], logN_Be[ind_both2], logN_Li[ind_both2]-0.2,
       logN_Be[ind_both2],length=0.1)
arrows(logN_Li[ind_both2], logN_Be[ind_both2], logN_Li[ind_both2],
       logN_Be[ind_both2]-0.1,length=0.1)
points(logN_Li[ind_both1], logN_Be[ind_both1], pch=16)
points(logN_Li[ind_both2], logN_Be[ind_both2], pch=1)

# Bivariate correlation and regression using Akritas-Thiel-Sen procedure
logN_Li1 <- logN_Li[-c(1,23)] # remove two points with NaN entries
Ind_Li1 <- Ind_Li[-c(1,23)] ;

logN_Be1 <- logN_Be[-c(1,23)]
Ind_Be1 <- Ind_Be[-c(1,23)]

Li_cen <- seq(FALSE, FALSE, length=66) # construct censoring indicator variables
Li_cen[which(Ind_Be1==0)] <- TRUE

Be_cen=seq(FALSE, FALSE, length=66)
Be_cen[which(Ind_Li1==0)] <- TRUE

cenken_out <- cenken(logN_Be1, Be_cen, logN_Li1, Li_cen)
abline(a=cenken_out$intercept, b=cenken_out$slope, lwd=2)

