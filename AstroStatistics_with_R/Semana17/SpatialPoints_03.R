#Spatial Points in R
#Let’s begin by creating a set spatial points layer from
#scratch. We’ll use the sp package to make a SpatialPoints 
#object using randomly generated XY coordinates. Once we 
#create a SpatialPoints object in R - we’ll take a closer 
#look at its metadata and structure.
# load library
library(sp)

# Generate 100 random X and Y coordinates 
# with longitude and latitude in the familiar
# degrees
x_coords <- runif(n = 100, min = -100, max = -80)
y_coords <- runif(n = 100, min = 25, max = 45)

# Have a look at the first coordinates
head(cbind(x_coords,y_coords))

#Now that we have generated random coordinates we can make 
#those data spatially explicit. We’ll use the SpatialPoints 
#function in the sp package to do that. Before we use the 
#function let’s see what arguments we need to pass to 
#SpatialPoints.
args("SpatialPoints")

#The coords input looking for a specific type of input. It 
#needs a matrix or data.frame where the first column is 
#longitude and second is latitude. Note - the order is 
#LONGITUDE then LATITUDE.
# coords = c(longitude,latitude)

firstPoints <- SpatialPoints(coords = cbind(x_coords,y_coords))

#Let’s have a look at what we just created.
str(firstPoints)
#Both proj4string and bbox have preset values so we don’t 
#need to specify them - it’ll use the defaults.

#We can plot the points in space by simply using the plot 
#function.
plot(firstPoints, pch = 19)

#One task that may be useful now that we have spatial points 
#is calculating the distance between points either within 
#the same layer or between two layers. Since we already have 
#a SpatialPoints file, we’ll calculate the distance between 
#points. We need to be a little careful here. First, we need 
#to make sure we’re calculating the distance we intend to 
#calculate. Let’s start simple and get Euclidean distances 
#in the units of the SpatialPoints layer. If in meters, it’ll 
#return meters. If degrees, it’ll return degrees.

# longlat = FALSE returns Euclidean distance
euclidDist <- sp::spDists(firstPoints,longlat = FALSE)
euclidDist

hist(euclidDist)

#Another option is to calculate the GreatCircle distance.

# longlat = TRUE returns GreatCircle distance
gcDist <- sp::spDists(firstPoints,longlat = TRUE)
gcDist

hist(gcDist)
