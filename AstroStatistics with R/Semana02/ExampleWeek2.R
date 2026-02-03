head(faithful)
duration = faithful$eruptions
range(duration)
breaks = seq(1.5, 5.5, by=0.5)
breaks
duration.cut = cut(duration, breaks, right=TRUE)
duration.freq = table(duration.cut)
duration.freq

hist(duration)
duration.relfreq = duration.freq / nrow(faithful)
hist(duration.relfreq)

duration.cumfreq = cumsum(duration.freq)
duration.cumfreq
cumfreq0 = c(0, cumsum(duration.freq))

plot(breaks, cumfreq0,main="Old Faithful Eruptions",xlab="Duration minutes",ylab="Cumumlative Eruptions")
lines(breaks, cumfreq0)


hist(duration,breaks=24)




Fn = ecdf(duration)
plot(Fn,main="Old Faithful Eruptions",xlab="Duration minutes",ylab="Cumumlative Proportion")

stem(duration)

waiting = faithful$waiting
plot(duration, waiting,xlab="Eruption duration",ylab="Time waited")
abline(lm(waiting ~ duration))


