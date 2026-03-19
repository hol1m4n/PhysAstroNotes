n = 30
sigma = 120
sem = sigma/sqrt(n)
sem

alpha = .05
mu0 = 10000
q = qnorm(alpha, mean=mu0, sd=sem)
q

mu = 9950
pnorm(q, mean=mu, sd=sem, lower.tail=FALSE)

# Si la potencia es muy grande es mas probable cometer un error tipo II

####################################################################

n = 35
sigma = 0.25
sem = sigma/sqrt(n)
sem

alpha = .05
mu0 = 2
q = qnorm(alpha, mean=mu0, sd=sem, lower.tail=FALSE)
q

mu = 2.09
pnorm(q, mean=mu, sd=sem)

####################################################################

n = 35
sigma = 2.5
sem = sigma/sqrt(n)
sem

alpha = .05
mu0 = 15.4
I = c(alpha/2, 1-alpha/2)
q = qnorm(I, mean=mu0, sd=sem)
q

mu = 15.1
p = pnorm(q, mean=mu, sd=sem)
p

diff(p)

####################################################################

n = 30
s = 125
SE = s/sqrt(n)
SE

alpha = .05
mu0 = 10000
q = mu0 + qt(alpha, df=n-1) * SE
q

mu = 9950
pt((q - mu)/SE, df=n-1, lower.tail=FALSE)

# Con un sigma mas grande la probabilidad de cometer el error tipo II es mas grande.



####################################################################

n = 35
s = 0.6
SE = s/sqrt(n)
SE

alpha = .05
mu0 = 2
q = mu0 + qt(alpha, df=n-1, lower.tail=FALSE) * SE
q

#x=seq(0.0,4.0,0.01)
y01=rnorm(401,2.09,0.6)
y02=rnorm(401,2.00,0.6)

plot(density(y01))
lines(density(y02),col='red')
abline(v=q)


####################################################################

n = 35
s = 2.5
SE = s/sqrt(n)
SE

alpha = .05
mu0 = 15.4
I = c(alpha/2, 1-alpha/2)
q = mu0 + qt(I, df=n-1) * SE
q




mu = 15.1
p = pt((q - mu)/SE, df=n-1)
p

diff(p)

####################################################################

library(MASS)
head(immer)

t.test(immer$Y1, immer$Y2, paired=TRUE)

mtcars$mpg

mtcars$am

L = mtcars$am == 0
mpg.auto = mtcars[L,]$mpg
mpg.auto

mpg.manual = mtcars[!L,]$mpg
mpg.manual

t.test(mpg.auto, mpg.manual)

plot(density(mpg.manual))
lines(density(mpg.auto))

t.test(mpg ~ am, data=mtcars)





hip <- read.table("~/Downloads/Semana06/Inciso/HIP_star.dat", header=T, fill=T)
attach(hip)

filter1 <- (RA>50 & RA<100 & DE>0 & DE<25)
filter2 <- (pmRA>90 & pmRA<130 & pmDE>-60 & pmDE< -10)
filter <- filter1 & filter2 & (e_Plx<5)
sum(filter)

color <- B.V
boxplot(color~filter,notch=T)

H <- color[filter]
nH <- color[!filter & !is.na(color)]

t.test(H,nH)

v1 <- var(H)/92
v2 <- var(nH)/2586
c(var(H),var(nH))

tstat <- (mean(H)-mean(nH))/sqrt(v1+v2)
tstat

(v1 + v2)^2 / (v1^2/91 + v2^2/2585)

2*pt(tstat,97.534)

####################################################################

library(MASS)
head(quine)

table(quine$Eth, quine$Sex)

prop.test(table(quine$Eth, quine$Sex), correct=FALSE)


