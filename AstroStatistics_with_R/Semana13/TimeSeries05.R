setwd('/home/holman/PhysAstroNotes/AstroStatistics_with_R/Semana13/')

Solar <- scan("SolarFlares.dat")

class(Solar)

start(Solar); end(Solar); frequency(Solar)

#The first step in time series data modeling using R is to convert the 
#available data into time series data format. To do so we need to run 
#the following command in R:
tsData = ts(Solar, start = c(1965,1), frequency = 12)

#We can use the following R code to find out the components of this time series:
#1.- Observed – the actual data plot
#2.- Trend – the overall upward or downward movement of the data points
#3.- Seasonal – any monthly/yearly pattern of the data points
#4.- Random – unexplainable part of the data
components.ts = decompose(tsData)
plot(components.ts)

#Various plots and functions that help in detecting seasonality:
#1.- A seasonal subseries plot
#2.- Multiple box plot
#3.- Auto correlation plot
#4.- ndiffs() is used to determine the number of first differences 
#required to make the time series non-seasonal
library("fUnitRoots")
urkpssTest(tsData, type = c("tau"), lags = c("short"),use.lag = NULL, doplot = TRUE)
tsstationary = diff(tsData, differences=1)
plot(tsstationary)

#R codes to calculate autocorrelation:
acf(tsData,lag.max=34) 

#To remove seasonality from the data, we subtract the seasonal component 
#from the original series and then difference it to make it stationary.
timeseriesseasonallyadjusted <- tsData - components.ts$seasonal
tsstationary <- diff(timeseriesseasonallyadjusted, differences=1)
plot(tsstationary)

#Once the data is ready and satisfies all the assumptions of modeling, to 
#determine the order of the model to be fitted to the data, we need three 
#variables: p, d, and q which are non-negative integers that refer to the 
#order of the autoregressive, integrated, and moving average parts of the 
#model respectively.
#To examine which p and q values will be appropriate we need to run acf() 
#and pacf() function.
#pacf() at lag k is autocorrelation function which describes the correlation 
#between all data points that are exactly k steps apart- after accounting 
#for their correlation with the data between those k steps. It helps to 
#identify the number of autoregression (AR) coefficients(p-value) in an 
#ARIMA model.
acf(tsstationary, lag.max=34)
pacf(tsstationary, lag.max=34)

#"order" specifies the non-seasonal part of the ARIMA model: (p, d, q) refers 
#to the AR order, the degree of difference, and the MA order.
#"seasonal" specifies the seasonal part of the ARIMA model, plus the period 
#(which defaults to frequency(x) i.e 12 in this case). This function requires 
#a list with components order and period, but given a numeric vector of 
#length 3, it turns them into a suitable list with the specification as the 
#‘order’.
#"method" refers to the fitting method, which can be ‘maximum likelihood(ML)’ 
#or ‘minimize conditional sum-of-squares(CSS)’. The default is 
#conditional-sum-of-squares.
fitARIMA <- arima(tsData, order=c(1,1,1),seasonal = list(order = c(1,0,0), period = 12),method="ML")

auto.arima(tsData, trace=TRUE)

fitARIMA <- arima(tsData, order=c(2,1,1),seasonal = list(order = c(0,1,0), period = 12),method="ML")

library(lmtest)
coeftest(fitARIMA) 

#We can use a function confint() for this purpose.
confint(fitARIMA)

acf(fitARIMA$residuals)

library('forecast')

predict(fitARIMA,n.ahead = 72)
futurVal <- forecast(fitARIMA,h=10, level=c(99.5))
plot(futurVal,xlim=)

