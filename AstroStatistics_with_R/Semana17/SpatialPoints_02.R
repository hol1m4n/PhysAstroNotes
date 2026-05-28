#Package sp is loaded by
library(sp)

df = data.frame(z1 = round(5 +rnorm(10), 2), z2 = 20:29)
df

xy.spdf = SpatialPointsDataFrame(xy.sp,df)
xy.spdf

summary(xy.spdf)

xc = round(runif(10), 2)
yc = round(runif(10), 2)
xy = cbind(xc, yc)
xy

df1 = data.frame(xy,df)
coordinates(df1) = c("xc", "yc")
df1

bubble(df1, "z1", key.space = "bottom")
spplot(df1, "z1", key.space = "bottom")
