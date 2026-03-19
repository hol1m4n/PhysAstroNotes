#Radio de la orbita en 10^6 km
#Periodo en días
radio=c(58,108,149,228,778,1430, 2870, 4500)
periodo=c(88,225,365,688,4329,10764,30660,60225)

plot(periodo ~ radio)

lin_mod = lm(periodo ~ radio)

#Plotting the model
plot(periodo ~ radio)
abline(lin_mod)

#a is the starting value and b is the exponential start
nonlin_mod=nls(log10(periodo) ~ b+a*log10(radio),start=list(a=1.555,b=0.1)) 

#T^2=cte1*R^3
#T=cte2*R^3/2
#Log(T)=cte3+Log(R^3/2)
#Log(T)=cte3+(3/2)*Log(R)
plot(log10(periodo)~log10(radio))
lines(log10(radio),predict(nonlin_mod),col="red")

