
#Tarea de la semana7, hacer esto mismo pero con cualquiero otro cumulo de Abell extraido de NED

#Corrieron Kepler_linear model,Keplernonlinear model no lo corrieron, Polylago y el de Abel si los corrieron.

######################################################################
#Plotting the kernel-smoothed distribution of galaxies in a cluster

######################################################################

#The dataset is a list of objects around the galaxy cluster Abell 85,
#obtained with an extended NED (NASA Extragalactic Database) search 
#with bar (pipe) separated ASCII output.

A <- read.table("~/Downloads/Semana07/a85_extended_NEDsearch.txt", sep="|", skip=20, header=TRUE)
dim(A)                                       # Show dimensions of data frame: rows columns

#The default column names are a bit of a mouthful, so let's rename some 
#of the ones to be used (object name, right ascension, declination & type):

#Show columns names
colnames(A)[c(2, 3, 4, 5)]

#Changing columns names
colnames(A)[c(2, 3, 4, 5)] <- c("name", "ra", "dec", "type")

# table summary of the same information
table(A$type)

# ordered bar chart
barplot(sort(table(A$type), decreasing=TRUE), log="y")

#You can show boxplots of the redshifts of each object type as follows:
boxplot(Redshift ~ type, data=A, log="y")
# Show redshift of the cluster Abell 85
abline(h=0.055, col="red")

#You can mark the redshifts of all objects identified as galaxy clusters 
#with a rug of dashes on the right axis:

rug(A$Redshift[A$type=="GClstr"], col="blue", side=4)

#You can see there are a few high redshift quasars, but also that the 
#galaxies form a very broad distribution, indicating that there is more 
#than 1 galaxy cluster present in the dataset.

#You can list the objects classified as galaxy clusters by NED:


A[A$type=="GClstr", ]
# list all types of object in the dataset
levels(A$type)

#Now plot the positions of the objects, using "." as a small point marker,
#as there are many objects, then zoom in on the interesting region, by 
#excluding the spatial outlier galaxies.

plot(dec ~ ra, data=A, pch=".", xlim=rev(range(min(ra),max(ra))))
plot(dec ~ ra, data=A, pch=".", xlim=rev(range(min(ra),max(ra))), ylim=c(-10, -9))
A <- subset(A, ra > 9.5 & ra < 11.5 & dec > -10.3 & dec < -8.5)
plot(dec ~ ra, data=A, pch=".", xlim=rev(range(min(ra),max(ra))))

# - make sure to leave space between "<" and "-" with negative numbers! 

#Now, let's focus on the galaxies only:

G <- subset(A, type=="G")

#and further limit ourselves to those galaxies with measured redshifts below 0.3 
#(when a value is missing in the input table, R assigns it the special value of NA; 
#see NA for more information)


G <- subset(G, !is.na(Redshift) & Redshift < 0.2)

#A histogram of the measured galaxy redshifts shows the main Abell 85 cluster peak 
#around 0.05 and a long tail up to z~0.25, which can be more clearly seen in a smoothed 
#density plot.

hist(G$Redshift)
plot(density(G$Redshift))
rug(G$Redshift)        # plot raw values as tick marks above X-axis

#Now let's create a new column in the data frame, containing a colour for each galaxy, 
#based on a simple redshift cut (red for higher redshift; blue for lower):

G$cols <- as.character(ifelse(G$Redshift > 0.1, "red", "blue"))
# Show the first few colours in the column
head(G$cols)
# Show all pre-defined colours ("colors()" is a valid alias)
colours()

# "cols" is retrieved from the data frame "G"
plot(dec ~ ra, data=G, col=cols, pch=20, xlim=rev(range(min(ra),max(ra))))

#There's some indication of an overdensity of more distant galaxies in the bottom right, 
#but it's hard to tell with only a crude redshift cut. A nicer result can be achieved by 
#expressing the redshift as a continously-varying colour.

#To do this, you'll need to define the following functions:

remap <- function(x) ( x - min(x) ) / max( x - min(x) )    # map x onto [0, 1]
fun.col <- function(x) rgb(colorRamp(c("blue", "red"))(remap(x)), maxColorValue = 255)


G$cols <- with(G, fun.col(Redshift) )

plot(dec ~ ra, data=G, col=cols, pch=20, xlim=rev(range(min(ra),max(ra))))

#You can also overlay a contour plot of a kernel-smoothed density estimate of the 
#galaxy distribution, using bkde2D from the package KernSmooth.

require(KernSmooth)     # ensure that the package is loaded

#--Calculate 2-dimensional kernel-smoothed estimate:
est <- bkde2D(G[c("ra", "dec")], bandwidth=c(0.07, 0.07), gridsize=c(101, 101))

#--Display as a contour map:
with(est, contour(x1, x2, fhat, drawlabels=FALSE, add=TRUE))

#This should produce a plot a bit like this one from the R plot gallery.


######################################################################
#Finding Substructures in Abell 85
######################################################################

#If don't have these packages, you can install them
#install.packages("packages")

library(rpanel)
library(misc3d) 
library(sm)
library(KernSmooth)
library(shape)
library(rgl)

#install.packages("tkrplot")

A85=read.csv("~/Downloads/Semana07/Tabla_Master_A85_fin.csv") #Read the CSV table
attach(A85)                                                   #Attach objects name into R
names(A85)                                                   #Check column names

#Compute the  "smoothing" parameter for the density kernel
#Two ways of compute h, Sheather-Jones Method plug-in:

#For Right Ascention ("RA")
h.select(RA,method="sj")
hsj(RA)

#The same for Declination ("Dec") and Radial Velocity ("Vel")
h.select(Dec,method="sj")
hsj(Dec)

h.select(Vel,method="sj")
hsj(Vel)

# Creation of density matrix using kernel smoothed with amplitudes calculate
# before. Using the function bkde2D from KernSmooth packages, we called 
# A85_SJ where SJ stand for Sheather-Jones.

A85_SJ=bkde2D(cbind(RA,Dec),bandwidth=c(0.05720054,0.08188228),gridsize=c(300,300))

# Make the graphic for density patter in the  RA,Dec plane.

plot(RA,Dec,xlim=rev(range(min(RA),max(RA))),pch=20,main="h = Sheather-Jones plug-in")
image(A85_SJ$x1,A85_SJ$x2,A85_SJ$fhat,add=T,col=shadepalette(10,"grey50","ghostwhite"))
points(RA,Dec,xlim=rev(range(min(RA),max(RA))),pch=1,cex=Delta)
points(10.46031,-9.30311,pch=4,cex=1)
box()

# Search the substructures of RA,Dec plane in 3D using "sm" packages

# To play with the 3D image, move the h parameter to see the "smoothing" effect

options(rgl.printRglwidget = TRUE)

sm.density(cbind(RA,Dec,Vel),panel=T)

install.packages("tkrplot")

# final vector of "smoothing" parameters
# Using the Sheather-Jones Method plug-in for RA, Dec, and Vel
# RA,Dec,Vel of A85: h=c(0.05720054,0.08188228,278.8725)

# To play with the 3D image, move the h parameter to see the "smoothing" effect

sm.density(cbind(RA,Dec,Vel),h=c(0.05720054,0.08188228,278.8725), panel=T)

