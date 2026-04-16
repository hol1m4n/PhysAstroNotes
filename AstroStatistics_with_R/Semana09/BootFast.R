#Fast bootstrap resampling to estimate regression parameter errors

#Bootstrap resampling is a very useful method to determine parameter error estimates. 
#This section makes use of the "boot" R package, which can be loaded with "library(boot)" or "require(boot)".

#A demonstration using simple linear regression:
set.seed(123)                                  # allow reproducible random numbers
N <- 20
A <- data.frame(x=1:N, y=rnorm(N, mean=1:N))   # create some data
plot(y ~ x, A)                                 # plot the data
m <- lm(y ~ x, A)                              # fit linear model
abline(m)                                      # add best-fit model to plot as a line
summary(m)                                     # show model best fit values & standard errors

#--Create a simple function to return the best-fit coefficients for the model 
#  fitted to a subset of the original data ("A"), given a vector of row 
#  numbers for the data frame ("indices"). "indices" will be the same length 
#  as "nrow(A)", and will be supplied by the "boot" function, using random 
#  sampling with replacement (i.e. "sample(nrow(A), replace=TRUE)")

mystat <- function(A, indices) {
  m <- lm(y ~ x, A[indices, ])
  return(coef(m))
}

#--Demonstrate function:
mystat(A, 1:nrow(A))                       # same as "coef(m)"

set.seed(123)                              # allow reproducible random numbers
mystat(A, sample(nrow(A), replace=TRUE))   # result for a single resample


#--Run full set of "N.boot" bootstrap resamples:
N.boot <- 1500
require(boot)                              # load boot library
set.seed(123)                              # allow reproducible random numbers
b <- boot(A, mystat, R=N.boot)

#--Plot results of bootstrapping:
plot(b, index=1)                          # intercept
plot(b, index=2)                          # slope

sdt2 = sd(b$t[,2])

hist(b$t[,2])
abline(v=b$t0[2],col='red',lwd=4)
abline(v=b$t0[2]+sdt2,col='blue',lwd=4)
abline(v=b$t0[2]-sdt2,col='blue',lwd=4)

sdt1 = sd(b$t[,1])

hist(b$t[,1])
abline(v=b$t0[1],col='red',lwd=4)
abline(v=b$t0[1]+sdt1,col='blue',lwd=4)
abline(v=b$t0[1]-sdt1,col='blue',lwd=4)

#--Now compare the standard errors on the model parameters from
#  the bootstrap resampling with those from the normal summary method:
b                                         # print results

#--Now show the standard errors computed (see "?summary.lm"):
coef(summary(m))

