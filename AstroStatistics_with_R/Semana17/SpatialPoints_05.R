# load library
library(sp)

# Make a set of coordinates that represent vertices
# with longitude and latitude in the familiar
# degrees
x_coords <- c(-60,-60,-62,-62,-60)
y_coords <- c(20,25,25,20,20)

#Next we use the Polygon function in the sp package to 
#make a polygon from our matrix of vertices
poly1 <- sp::Polygon(cbind(x_coords,y_coords))

#Then we make poly1 into a Polygon class using the Polygons function
firstPoly <- sp::Polygons(list(poly1), ID = "A")

str(firstPoly,1)

#Then we can make firstPoly into a SpatialPolygons
firstSpatialPoly <- sp::SpatialPolygons(list(firstPoly))

firstSpatialPoly

#We can create two or more polygons into a single SpatialPolygon 
#file as well. That workflow looks something like this:
# define the vertices
x1 <- c(-60,-60,-62,-62,-60)
x2 <-c(-50,-50,-55,-55,-50)
y1 <- c(20,25,25,20,20)
y2 <- c(15,25,25,15,15)

# assign the vertices to a `polygon` 
poly1 <- sp::Polygon(cbind(x1,y1))
poly2 <- sp::Polygon(cbind(x2,y2))

# This step combines the last two together - making Polygons and then SpatialPolygons
TwoPolys <- sp::SpatialPolygons(list(sp::Polygons(list(poly1),ID = "A"),
                                     sp::Polygons(list(poly2), ID = "B")))

#Let's take a look
TwoPolys

plot(TwoPolys)
