setwd('~/Downloads/Semana11/')

data = read.table('spec155161.dat',header=TRUE)
attach(data)
head(data)

plot(l,fo,type='l')
plot(l,fo,type='l', xlim = c(4965,5045), ylim=c(0,4))

# Ventana del continuo
lw1 = l[which(l >= 4970 & l <= 4980)]
fw1 = fo[which(l >= 4970 & l <= 4980)]

lw2 = l[which(l >= 5020 & l <= 5030)]
fw2 = fo[which(l >= 5020 & l <= 5030)]

# SNR en cada ventana
snw1 = mean(fw1)/sd(fw1)
snw2 = mean(fw2)/sd(fw2)

# Ventana de la linea
lOIII = l[which(l >= 4981 & l <= 5019)]
fOIII = fo[which(l >= 4981 & l <= 5019)]

#Hacemos un modelo lineal del continuo
mod1 = lm(fw1 ~ lw1)
summary(mod1)

mod2 = lm(fw2 ~ lw2)
summary(mod2)

newf_fit = c(fw1,fw2)
newl_fit = c(lw1,lw2)

mod3 = lm(newf_fit ~ newl_fit)

fmod1 = mod1$coefficients[2]*lOIII + mod1$coefficients[1]
fmod2 = mod2$coefficients[2]*lOIII + mod2$coefficients[1]
fmod3 = mod3$coefficients[2]*lOIII + mod3$coefficients[1]

# Continuo promedio
cont_mean = (fmod1 + fmod2)/2
lcont = c(lw1,lw2)
fcont = c(fw1,fw2)
  
fmod1_cont = mod1$coefficients[2]*lcont + mod1$coefficients[1]
fmod2_cont = mod2$coefficients[2]*lcont + mod2$coefficients[1]

F_cont = (rowMeans(cbind(fmod1_cont,fmod2_cont)))
disp_Cont = mean(abs(fcont - F_cont))

F_linea = fOIII - cont_mean
F_linea

Signal = sum(F_linea)

Noise = disp_Cont
Noise

#Distribución de Poisson normalizada
Npix <- length(fOIII)
pk<- rpois(n=Npix, lambda = 1)        #Distribución aleatoria
Pk <- (pk-min(pk))/(max(pk)-min(pk))  #Normalizada
print(sd(Pk)/mean(Pk))                #comprobación de normalización

#ruido de Poisson de la línea
np <- Pk*sqrt(fOIII)

Noise_final <- sqrt(disp_Cont^2 + sd(np)^2)
SNR_final <- Signal/Noise_final

SNR_final

plot(l,fo,type='l', xlim = c(4970,5035), ylim=c(0.5,3.0))
lines(lOIII,fmod3,col='green',lwd=4)
lines(lOIII,fmod3+Noise,col='blue',lwd=2)
lines(lOIII,fmod3+2*Noise,col='blue',lwd=2)
lines(lOIII,fmod3+3*Noise,col='blue',lwd=2)
lines(lOIII,fmod3+10*Noise,col='blue',lwd=2)

#Calcular todo esto pero para Hbeta. 4861 en Ang.

setwd('~/Downloads/Semana11/')

data = read.table('spec155161.dat',header=TRUE)
attach(data)
head(data)

plot(l,fo,type='l')
plot(l,fo,type='l', xlim = c(4965,5045), ylim=c(0,4))


# Ventana del continuo
lw1 = l[which(l >= 4840 & l <= 4880)]
fw1 = fo[which(l >= 4840 & l <= 4880)]

lw2 = l[which(l >= 5020 & l <= 5030)]
fw2 = fo[which(l >= 5020 & l <= 5030)]

# SNR en cada ventana
snw1 = mean(fw1)/sd(fw1)
snw2 = mean(fw2)/sd(fw2)

# Ventana de la linea
lOIII = l[which(l >= 4850 & l <= 5870)]
fOIII = fo[which(l >= 4850 & l <= 5870)]

#Hacemos un modelo lineal del continuo
mod1 = lm(fw1 ~ lw1)
summary(mod1)

mod2 = lm(fw2 ~ lw2)
summary(mod2)

newf_fit = c(fw1,fw2)
newl_fit = c(lw1,lw2)

mod3 = lm(newf_fit ~ newl_fit)

fmod1 = mod1$coefficients[2]*lOIII + mod1$coefficients[1]
fmod2 = mod2$coefficients[2]*lOIII + mod2$coefficients[1]
fmod3 = mod3$coefficients[2]*lOIII + mod3$coefficients[1]

# Continuo promedio
cont_mean = (fmod1 + fmod2)/2
lcont = c(lw1,lw2)
fcont = c(fw1,fw2)

fmod1_cont = mod1$coefficients[2]*lcont + mod1$coefficients[1]
fmod2_cont = mod2$coefficients[2]*lcont + mod2$coefficients[1]

F_cont = (rowMeans(cbind(fmod1_cont,fmod2_cont)))
disp_Cont = mean(abs(fcont - F_cont))

F_linea = fOIII - cont_mean
F_linea

Signal = sum(F_linea)

Noise = disp_Cont
Noise

#Distribución de Poisson normalizada
Npix <- length(fOIII)
pk<- rpois(n=Npix, lambda = 1)        #Distribución aleatoria
Pk <- (pk-min(pk))/(max(pk)-min(pk))  #Normalizada
print(sd(Pk)/mean(Pk))                #comprobación de normalización

#ruido de Poisson de la línea
np <- Pk*sqrt(fOIII)

Noise_final <- sqrt(disp_Cont^2 + sd(np)^2)
SNR_final <- Signal/Noise_final

SNR_final

plot(l,fo,type='l', xlim = c(4970,5035), ylim=c(0.5,3.0))
lines(lOIII,fmod3,col='green',lwd=4)
lines(lOIII,fmod3+Noise,col='blue',lwd=2)
lines(lOIII,fmod3+2*Noise,col='blue',lwd=2)
lines(lOIII,fmod3+3*Noise,col='blue',lwd=2)
lines(lOIII,fmod3+10*Noise,col='blue',lwd=2)


# Tarea correr el Hb y correr lo de la Fosfina.






