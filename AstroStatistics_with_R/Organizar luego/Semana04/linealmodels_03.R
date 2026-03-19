## The example data files are described in more detail in the
## \dQuote{Model II User's guide, R edition} tutorial.

library(lmodel2)

## Example 1 (surgical unit data)
data(mod2ex1)
Ex1.res <- lmodel2(Predicted_by_model ~ Survival, data=mod2ex1, nperm=99)
Ex1.res
plot(Ex1.res)

xf1 = seq(0,3,0.1)
yf1 = Ex1.res$regression.results$Slope[1]*xf1 + Ex1.res$regression.results$Intercept[1]
yf2 = Ex1.res$regression.results$Slope[2]*xf1 + Ex1.res$regression.results$Intercept[2]
yf3 = Ex1.res$regression.results$Slope[3]*xf1 + Ex1.res$regression.results$Intercept[3]

plot(mod2ex1$Survival,mod2ex1$Predicted_by_model)
lines(xf1,yf1, col='blue', lwd=2)
lines(xf1,yf2, col='red', lwd=2)
lines(xf1,yf3, col='green', lwd=2)
