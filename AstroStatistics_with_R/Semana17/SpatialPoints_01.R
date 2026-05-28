#Package sp is loaded by
library(sp)

#We can generate a set of 10 points on the unit square [0,1]×[0,1] by
xc = round(runif(10), 2)
yc = round(runif(10), 2)
xy = cbind(xc, yc)
xy

#This 10×2 matrix can be converted into a SpatialPoints object by
xy.sp = SpatialPoints(xy)
xy.sp

plot(xy.sp, pch = 2)

#We can retrieve the coordinates from xy.sp by
xy.cc = coordinates(xy.sp)
class(xy.cc)

dim(xy.cc)

summary(xy.sp)
