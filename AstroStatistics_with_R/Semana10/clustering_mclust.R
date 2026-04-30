library(mclust)

data(diabetes)
head(diabetes)

class <- diabetes$class
table(class)
X <- diabetes[,-1]
head(X)

clPairs(X, class)

library(mclust)
BIC <- mclustBIC(X)
plot(BIC)

summary(BIC)

mod1 <- Mclust(X, x = BIC)
summary(mod1, parameters = TRUE)

plot(mod1, what = "classification")

#Singular Value Decomposition
(hc1 <- hc(X, modelName = "VVV", use = "SVD"))

BIC1 <- mclustBIC(X, initialization = list(hcPairs = hc1)) # default 
summary(BIC1)

#VARS
(hc2 <- hc(X, modelName = "VVV", use = "VARS"))

BIC2 <- mclustBIC(X, initialization = list(hcPairs = hc2))
summary(BIC2)

(hc3 <- hc(X, modelName = "EEE", use = "SVD"))

BIC3 <- mclustBIC(X, initialization = list(hcPairs = hc3))
summary(BIC3)

BIC <- mclustBICupdate(BIC1, BIC2, BIC3)
summary(BIC)

plot(BIC)

##############################################
load("/home/torrespapaqui/CursoR/Semana08/Hidalgo1872.rda")
Thickness <- Hidalgo1872$thickness
Year <- rep(c("1872", "1873-74"), c(289, 196))

library(mclust)
dens <- densityMclust(Thickness)
summary(dens$BIC)

summary(dens, parameters = TRUE)

br <- seq(min(Thickness), max(Thickness), length = 21)
plot(dens, what = "density", data = Thickness, breaks = br)

h1 <- hist(Thickness[Year == "1872"], breaks = br, plot = FALSE)
h1$density <- h1$density*prop.table(table(Year))[1]
h2 <- hist(Thickness[Year == "1873-74"], breaks = br, plot = FALSE)
h2$density <- h2$density*prop.table(table(Year))[2]
x <- seq(min(Thickness)-diff(range(Thickness))/10,
         max(Thickness)+diff(range(Thickness))/10, length = 200)
cdens <- predict(dens, x, what = "cdens")
cdens <- t(apply(cdens, 1, function(d) d*dens$parameters$pro))
col <- adjustcolor(mclust.options("classPlotColors")[1:2], alpha = 0.3)
plot(h1, xlab = "Thickness", freq = FALSE, main = "", border = FALSE, col = col[1],
      xlim = range(x), ylim = range(h1$density, h2$density, cdens))
plot(h2, add = TRUE, freq = FALSE, border = FALSE, col = col[2])
matplot(x, cdens, type = "l", lwd = 1, add = TRUE, lty = 1:3, col = 1)
box()
