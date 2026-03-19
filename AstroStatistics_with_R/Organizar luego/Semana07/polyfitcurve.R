x <- c(32,64,96,118,126,144,152.5,158)  
y <- c(99.5,104.8,108.5,100,86,64,35.3,15)

plot(x,y)

library(pracma)

p0 = polyfit(x,y,0)
p1 = polyfit(x,y,1)
p2 = polyfit(x,y,2)
p3 = polyfit(x,y,3)
p4 = polyfit(x,y,4)
p5 = polyfit(x,y,5)

plot(x,y,ylim = c(0,140))
lines(x, polyval(p0, x), col = "blue")
lines(x, polyval(p1, x), col = "red")
lines(x, polyval(p2, x), col = "green")
lines(x, polyval(p3, x), col = "magenta")
lines(x, polyval(p4, x), col = "blue")
lines(x, polyval(p5, x), col = "red")
