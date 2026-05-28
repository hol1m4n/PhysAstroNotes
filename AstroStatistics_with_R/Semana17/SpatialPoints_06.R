library(raster)
library(sp)
library(geosphere)

# generate 100 random XY coords 
x_coords <- runif(n = 100, min = -100, max = -80)
y_coords <- runif(n = 100, min = 25, max = 45)

# make a unique identifier for each line
ID <- paste0("line_",1:100)
ID

#Now that we have the endpoints, we need to create a Line 
#object then convert that to a Lines object and give them 
#an ID then we can finally convert them to SpatialLines. 
#Here, we’ll use the first two points for our first line.

line_obj <- sp::Line(cbind(x_coords[1:2],y_coords[1:2]))

lines_obj <- sp::Lines(list(line_obj),ID=ID[1])

firstLine <- sp::SpatialLines(list(lines_obj))

# make SpatialPoints
points <- sp::SpatialPoints(cbind(x_coords,y_coords))

plot(points)

# use as to convert to line
sp_line <- as(points,"SpatialLines")

plot(sp_line)

# great circle distance along our line
geosphere::lengthLine(sp_line)

