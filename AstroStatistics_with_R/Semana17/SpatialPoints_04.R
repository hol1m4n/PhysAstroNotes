#remotes::install_github('rspatial/rspatial')
library(rspatial)

city <- sp_data('city')
crime <- sp_data('crime')

#Here is a map of both datasets.
plot(city, col='light blue')
points(crime, col='red', cex=.5, pch='+')

#A sorted table of the incidence of crime types.
tb <- sort(table(crime$CATEGORY))[-1]
tb

#Let’s get the coordinates of the crime data, and for this exercise, 
#remove duplicate crime locations. These are the ‘events’ we will use 
#below (later we’ll go back to the full data set).
xy <- coordinates(crime)
dim(xy)

xy <- unique(xy)
dim(xy)

head(xy)

#Compute the mean center and standard distance for the crime data.
# mean center
mc <- apply(xy, 2, mean)
mc

# standard distance
sd <- sqrt(sum((xy[,1] - mc[1])^2 + (xy[,2] - mc[2])^2) / nrow(xy))
sd

#Plot the data to see what we’ve got. I add a summary circle by dividing the 
#circle in 360 points and compute bearing in radians. I do not think this is 
#particularly helpful, but it might be in other cases. And it is always fun 
#to figure out how to do tis.
plot(city, col='light blue')
points(crime, cex=.5)
points(cbind(mc[1], mc[2]), pch='*', col='red', cex=5)

# make a circle
bearing <- 1:360 * pi/180
cx <- mc[1] + sd * cos(bearing)
cy <- mc[2] + sd * sin(bearing)
circle <- cbind(cx, cy)
lines(circle, col='red', lwd=2)

#Here is a basic approach to computing point density.
CityArea <- raster::area(city)
dens <- nrow(xy) / CityArea

#To compute quadrat counts I first create quadrats (a RasterLayer). I get the 
#extent for the raster from the city polygon, and then assign an an arbitrary 
#resolution of 1000. (In real life one should always try a range of resolutions, 
#I think).
r <- raster(city)
res(r) <- 1000
r

#To find the cells that are in the city, and for easy display, I create polygons 
#from the RasterLayer.
r <- rasterize(city, r)

plot(r)
quads <- as(r, 'SpatialPolygons')

plot(quads, add=TRUE)
points(crime, col='red', cex=.5)

#The number of events in each quadrat can be counted using the ‘rasterize’ 
#function. That function can be used to summarize the number of points within 
#each cell, but also to compute statistics based on the ‘marks’ (attributes). 
#For example we could compute the number of different crime types) by changing 
#the ‘fun’ argument to another function (see ?rasterize).
nc <- rasterize(coordinates(crime), r, fun='count', background=0)

plot(nc)
plot(city, add=TRUE)

#nc has crime counts. As we only have data for the city, the areas outside of 
#the city need to be excluded. We can do that with the mask function (see ?mask).
ncrimes <- mask(nc, r)

plot(ncrimes)
plot(city, add=TRUE)

#Better. Now the frequencies.
f <- freq(ncrimes, useNA='no')
head(f)

plot(f, pch=20)

