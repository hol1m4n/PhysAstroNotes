data(AirPassengers)

AP <- AirPassengers
AP

class(AP)

start(AP); end(AP); frequency(AP)

#One of the most important steps in a preliminary time series analysis
#is to plot the data; i.e., create a time plot. For a time series object,
#this is achieved with the generic plot function
plot(AP, ylab = "Passengers (1000's)")

#The plots can be put in a single graphics window using the layout
#function, which takes as input a vector (or matrix) for the location 
#of each plotin the display window.
plot(aggregate(AP))

#You can see an increasing trend in the annual series and the seasonal
#effects in the boxplot. More people travelled during the summer months
#of June to September.
boxplot(AP ~ cycle(AP))



#The monthly unemployment data are available in a file online
Maine.month <- read.table("Data/Maine.dat", header = TRUE)
attach(Maine.month)
class(Maine.month)

#When we read data in this way from an ASCII text file, the ‘class’ 
#is not time series but data.frame. The ts function is used to convert 
#the data to a time series object.
Maine.month.ts <- ts(unemploy, start = c(1996, 1), freq = 12)

#The average (mean) over the twelve months of each year is another
#example of aggregated data, but this time we divide by 12 to give 
#a mean annual rate.
Maine.annual.ts <- aggregate(Maine.month.ts)/12

plot(Maine.month.ts, ylab = "unemployed (%)")

plot(Maine.annual.ts, ylab = "unemployed (%)")

