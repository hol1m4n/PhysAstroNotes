

format(12345.6789)
format(12345.6789, digits = 3)
format(12345.6789, digits = 5)
format(12345.6789, digits = 7)
format(12345.6789, digits = 9)

format(pi,digits=4)
format(pi,digits=4,nsmall=5)

x <- colMeans(mtcars[, 1:4])

help(colMeans)

mtcars
help(mtcars)

format(x, digits=2, nsmall=2)
format(x, digits=2, nsmall=3)
format(x, digits=5, nsmall=3)
format(x, digits=5, nsmall=2)

x1 <- rowMeans(mtcars[, 3:4])
x1
format(x1, digits=2, nsmall=2)
format(x1, digits=2, nsmall=4)
format(x1, digits=2, nsmall=8)

z <- seq(0.5, 0.55, 0.01)
help(seq)
sprintf("%.1f %%", 100*z)
sprintf("%6.2f %%", 100*z)
sprintf("%9.2f %%", 100*z)

set.seed(1)
y <- 1000*runif(5)
sprintf("$ %3.2f", y)

stuff <- c("bread", "cookies")
price <- c(2.1, 4)
sprintf("%s costed $ %3.2f ", stuff, price)
sprintf("%s costed $ %5.2f ", stuff, price)

# print today's date
today <- Sys.Date()
format(today, format="%B %d %Y")

# Symbol 	Meaning 	Example
# %d 	day as a number (0-31) 	 01-31
# %a  abbreviated weekday      Mon
# %A 	unabbreviated weekday 	 Monday
# %m 	month (00-12) 	         00-12
# %b  abbreviated month        Jan
# %B 	unabbreviated month 	   January
# %y  2-digit year             07
# %Y 	4-digit year 	           2007

beta <- c(2.1, 6.21, 4.321, 5.4321)
SE <- c(5, 1.49, 11.13, 0.31)
t <- beta/SE
p.value <- 2*pt(-abs(t), 12)

x <- data.frame(beta, SE, t, p.value)
row.names(x) <- c("HS GPA", "college", "gender", "age")
x

fx <- format(x, digits=3)
fx
fx[,1]

setwd("/home/hollman/PhysAstroNotes/AstroStatistics with R/Semana01")

library(readxl)
mydata = read_excel("mydata.xls") # read from first sheet

mydata = read.table("mydata.txt") # read text file
mydata # print data frame

mydata = read.csv("mydata.csv") # read csv file
mydata # print data frame

