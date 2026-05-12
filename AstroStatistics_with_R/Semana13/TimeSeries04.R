setwd('/home/papaqui/CursoR/Semana13/')

# Read in GX 5-1 data and create time series
GX2ori.dat <- read.table("x1608-522_current.dat", header = TRUE)
GX2.dat <- GX2ori.dat$Fitted[1:57728]
GX2.time <- seq(from=0, to=451, length.out=57728)
GX2.ts <- ts(GX2.dat, GX2.time) ; GX2.ts.offset <- ts(GX2.dat-30, GX2.time)

# Compare histogram of counts to normal distribution
hist(GX2.dat, breaks=100, xlim=c(40,100), ylim=c(0,3500), xlab='GX 5-1 counts',
     font=2, font.lab=2, main='')
curve(dnorm(x,mean=mean(GX2.dat), sd=sqrt(mean(GX2.dat)))*57728, lwd=3, add=T)
sd(GX2.dat) / sqrt(mean(GX2.dat)) # result is 1.2417

# Examine raw and smoothed time series
plot.ts(GX2.ts[1:6000], ylab='GX 5-1 counts', xlab='Time (1/128 sec)',
        cex.lab=1.3, cex.axis=1.3)
plot(GX2.time,GX2.dat, ylim=c(-100,115), xlab='Time (sec)', ylab='GX 5-1 counts',
     cex.lab=1.3, cex.axis=1.3, type='n')
lines(ksmooth(GX2.time, GX2.dat+30, 'normal', bandwidth=7), lwd=2)
text(420, 55, 'Normal kernel')
lines(filter(GX2.ts, sides=2, rep(1,7)/7), lwd=2)
text(420, 15, 'Moving average')
lines(kernapply(GX2.ts.offset, kernel('modified.daniell', 7)), lwd=2)
text(420, -15, 'Modified Daniell')
lines(supsmu(GX2.time, GX2.dat-60), lwd=2)
text(400, -45, 'Friedman’s super-smoother')
lines(lowess(GX2.time, GX2.dat-80, 0.05), lwd=2)
text(420, -70, 'Local regression')

# Raw periodogram
f <- 0:28864/57728
I <- (4/57728) * abs(fft(GX2.ts) / sqrt(57728))^2
plot(f[2:60000], I[2:60000], type="l", ylab="Power", xlab="Frequency")
Pergram <- spec.pgram(GX2.ts,log='no',main='')
summary(Pergram)
Pergram$spec[500] # value of 500th point
2*Pergram$spec[500] / qchisq(c(0.025,0.975),2)

# Raw and smoothed periodogram
par(mfrow=c(3,1))
spec.pgram(GX2.ts, log='no', main='', sub='')
spec.pgram(GX2.ts, spans=50, log='no', main='', sub='')
spec.pgram(GX2.ts, spans=500, log='no', main='', sub='')
par(mfrow=c(1,1))

# Autocorrelation functions of the GX 5-1 time series
par(mfrow=c(1,2))
acf(GX2.ts, 40, main='', ci.col='black', ylim=c(-0.05,0.3), lwd=2)
pacf(GX2.ts, 40, main='', ci.col='black', ylim=c(-0.05,0.3), lwd=2)
par(mfrow=c(1,1))

# Autoregressive modeling
ARmod <- ar(GX2.ts, method='ols')
ARmod$order # model selection based on AIC
ARmod$ar # best-fit parameter values
ARmod$asy.se.coef$ar # parameter standard errors

plot(0:29, log10(ARmod$aic[1:30]), xlab='AR model parameters',
     ylab='log(AIC)', pch=20)
arrows(29, 1.4, 29, 1.0, length=0.1)

# Spectrum of AR model
ARspec <- spec.ar(GX2.ts, plot=F)
GXspec <- spec.pgram(GX2.ts, span=101, main='', sub='', lwd=2)
lines(ARspec$freq, ARspec$spec, col='red', lwd=4)
legend(0.23, 550, c('Periodogram, Daniell smooth', 'AR(27) model'),
       lty=c(1,1), lwd=c(2,4), col=c('black','red'))

# Spectral estimates of the long-range memory parameter d
library(fracdiff)
d.FARIMA <- fracdiff(GX2.ts, nar=27, nma=1, ar=ARmod$ar) ; d.FARIMA$d
d.GPH <- fdGPH(GX2.ts)
d.Reisen <- fdSperio(GX2.ts)