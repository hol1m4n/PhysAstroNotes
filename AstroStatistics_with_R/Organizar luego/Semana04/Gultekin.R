library(data.table)
dataBH = fread("/home/papaqui/CursoR/Semana04/Gultekin.dat")

library(ggplot2)
ggplot(dataBH, aes(x=sigma, y=log10(Mbh))) + geom_point()

# Add the regression line
ggplot(dataBH, aes(x=sigma, y=log10(Mbh))) +
  geom_point() +
  geom_smooth(method=lm)
  
# Add the y error bar
ggplot(dataBH, aes(x=sigma, y=log10(Mbh))) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_errorbar(aes(ymin=log10(Mlo), ymax=log10(Mhi)))

# Add the x,y error bars
ggplot(dataBH, aes(x=sigma, y=log10(Mbh))) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_errorbar(aes(ymin=log10(Mlo), ymax=log10(Mhi))) +
  geom_errorbarh(aes(xmin=sigma-serr, xmax=sigma+serr))

# fit with Morphology
ggplot(dataBH, aes(x=sigma, y=log10(Mbh), color=dataBH$Morpho)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_errorbar(aes(ymin=log10(Mlo), ymax=log10(Mhi))) +
  geom_errorbarh(aes(xmin=sigma-serr, xmax=sigma+serr))

# fit with Methodology
ggplot(dataBH, aes(x=sigma, y=log10(Mbh), color=dataBH$Method)) +
  geom_point() +
  geom_smooth(method=lm) +
  geom_errorbar(aes(ymin=log10(Mlo), ymax=log10(Mhi))) +
  geom_errorbarh(aes(xmin=sigma-serr, xmax=sigma+serr))

library(lmodel2)

modbh <- lmodel2(log10(dataBH$Mbh) ~ dataBH$sigma, data=dataBH, "interval", "interval", 99)

op <- par(mfrow = c(2,2))
plot(modbh, "OLS")
plot(modbh, "MA")
plot(modbh, "SMA")
plot(modbh, "RMA")
par(op)

plot(modbh, "OLS")
abline(a=modbh$regression.results[2,2],b=modbh$regression.results[2,3],col="blue")
abline(a=modbh$regression.results[3,2],b=modbh$regression.results[3,3],col="green")
abline(a=modbh$regression.results[4,2],b=modbh$regression.results[4,3],col="magenta")

library(latex2exp)

ggplot(dataBH, aes(x=sigma, y=log10(Mbh), color=dataBH$Method)) + geom_point() + 
  geom_abline(intercept=modbh$regression.results[1,2],slope=modbh$regression.results[1,3],colour="blue") +
  geom_abline(intercept=modbh$confidence.intervals[1,3],slope=modbh$confidence.intervals[1,4],colour="red") + 
  geom_abline(intercept=modbh$confidence.intervals[1,2],slope=modbh$confidence.intervals[1,5],colour="red") +
  ggtitle(expression(OLS)) + xlab(TeX("$\\sigma$ (km/s)")) + ylab(TeX("$\\log\\, (M_{BH}/M_{o})$"))

# Add the regression line
ggplot(dataBH, aes(x=MVB, y=log10(Mbh))) +
  scale_x_reverse() +
  geom_point() +
  geom_smooth(method=lm)
  
# Add the x,y error bars
ggplot(dataBH, aes(x=MVB, y=log10(Mbh), color=dataBH$Method)) +
  scale_x_reverse() +
  geom_point() +
  geom_smooth(method=lm) +
  geom_errorbar(aes(ymin=log10(Mlo), ymax=log10(Mhi))) +
  geom_errorbarh(aes(xmin=MVB-merr, xmax=MVB+merr)) +
  xlab(TeX("$M_V$ (Bulge)")) + ylab(TeX("$\\log\\, (M_{BH}/M_{o})$"))

modbul <- lmodel2(log10(dataBH$Mbh) ~ dataBH$MVB, data=dataBH, "interval", "interval", 99)

ggplot(dataBH, aes(x=MVB, y=log10(Mbh), color=dataBH$Method)) + geom_point() + 
  scale_x_reverse() +
  geom_abline(intercept=modbul$regression.results[1,2],slope=modbul$regression.results[1,3],colour="blue") +
  geom_abline(intercept=modbul$confidence.intervals[1,3],slope=modbul$confidence.intervals[1,4],colour="red") + 
  geom_abline(intercept=modbul$confidence.intervals[1,2],slope=modbul$confidence.intervals[1,5],colour="red") +
  xlab(TeX("$M_V$ (Bulge)")) + ylab(TeX("$\\log\\, (M_{BH}/M_{o})$"))

