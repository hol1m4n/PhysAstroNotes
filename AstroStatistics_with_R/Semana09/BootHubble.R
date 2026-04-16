Hubble = read.table("HubbleConstants.dat",h=TRUE)
attach(Hubble)

rhubble.gaussian <- function() {
  rnorm(n=nrow(Hubble),mean=mean(Hubble$Ho),sd=sd(Hubble$Ho))
}

mean.dist.gaussian <-replicate(3000,mean(rhubble.gaussian()))
m=mean(mean.dist.gaussian)
m

hist(mean.dist.gaussian,breaks=35,prob=TRUE,border="chartreuse", col="darkgreen")
lines(density(mean.dist.gaussian),col="red",lwd=2)
abline(v=m,lty=2,col="blue",lwd=4)
abline(v=m+sd(mean.dist.gaussian),lty=2,col="green",lwd=4)
abline(v=m-sd(mean.dist.gaussian),lty=2,col="green",lwd=4)
