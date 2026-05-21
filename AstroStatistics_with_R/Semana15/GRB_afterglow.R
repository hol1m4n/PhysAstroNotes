setwd("/home/holman/Downloads/Semana15")

data <- read.table("GRB_afterglow.dat", header = TRUE)

plot(data$t,data$f)

plot(log10(data$t),log10(data$f))

plot(log10(data$t),log10(data$f)-11.)

time = log10(data$t)
flux = log10(data$f)-11.
error = data$s

plot(time,flux)

fitx = nls(flux~a*time**b,start=list(a=-1.0,b=-0.9))

plot(time,flux)
lines(time,predict(fitx),col="red")

plot(time,flux)
lines(time,predict(fitx)-0.16,col="red")

plot(time[1:55],flux[1:55])

flux01 = flux[1:55]
time01 = time[1:55]
  
fitx01 = nls(flux01~a*time01**b,start=list(a=-1.0,b=-0.9))

plot(time,flux)
lines(time[1:55],predict(fitx01)-0.14,col="red")

flux02 = flux[55:63]
time02 = time[55:63]

fitx02 = nls(flux02~a*time02**b,start=list(a=-1.0,b=-0.9))

plot(time,flux)
lines(time[1:55],predict(fitx01)-0.14,col="red")
lines(time[55:63],predict(fitx02),col="red")

resid01 = flux[1:55] - (predict(fitx01)-0.14)
resid02 = flux[55:63] - (predict(fitx02))

resid03 = c(resid01,resid02[-1])

plot(time,resid03)
abline(h=0.0, col="red")

lm01 = lm(flux01 ~ time01)

plot(time[1:55],flux[1:55],ylim=c(-12,-8.8))
lines(time[1:55],predict(fitx01)-0.14,col="red")
abline(lm01,col="purple")

summary(lm01)

library(MASS)

lm02 = rlm(flux01 ~ time01)

plot(time[1:55],flux[1:55],ylim=c(-12,-8.8))
lines(time[1:55],predict(fitx01)-0.14,col="red")
abline(lm01,col="purple")
abline(lm02,col="blue")

summary(lm02)

flux03 = flux[1:17]
time03 = time[1:17]

fitx03 = nls(flux03~a*time03**b,start=list(a=-1.0,b=-0.9))

plot(time[1:49],flux[1:49],ylim=c(-10,-8.8))
lines(time[1:17],predict(fitx03),col="red")

flux04 = flux[17:19]
time04 = time[17:19]

fitx04 = nls(flux04~a*time04**b,start=list(a=-1.0,b=-0.9))

plot(time[1:49],flux[1:49],ylim=c(-10,-8.8))
lines(time[1:17],predict(fitx03),col="red")
lines(time[17:19],predict(fitx04),col="red")

flux05 = flux[19:49]
time05 = time[19:49]

fitx05 = nls(flux05~a*time05**b,start=list(a=-1.0,b=-0.9))

plot(time[1:49],flux[1:49],ylim=c(-10,-8.8))
lines(time[1:17],predict(fitx03),col="red")
lines(time[17:19],predict(fitx04),col="red")
lines(time[19:49],predict(fitx05),col="red")

plot(time,flux)
lines(time[1:17],predict(fitx03),col="red")
lines(time[17:19],predict(fitx04),col="red")
lines(time[19:49],predict(fitx05),col="red")
lines(time[1:55],predict(fitx01)-0.14,col="blue")
lines(time[55:63],predict(fitx02),col="blue")
abline(lm(flux~time),col="purple")
abline(rlm(flux~time),col="magenta")



# 1. Calcular y extraer los residuos de ambos modelos
residuos_lm01  <- residuals(lm01) # Residuos del modelo lineal ordinario
residuos_lm02  <- residuals(lm02) # Residuos del modelo lineal robusto (rlm)

# 2. Visualizar un resumen estadístico rápido de los residuos
summary(residuos_lm01)
summary(residuos_lm02)

# 3. Graficar los residuos para comparar visualmente sus distribuciones
boxplot(residuos_lm01, residuos_lm02, 
        names = c("Modelo Lineal", "Modelo Robusto"),
        main  = "Comparación de Residuos", 
        col   = c("purple", "blue"), 
        ylab  = "Valor del residuo")
abline(h = 0, col = "red", lty = 2) # Línea de referencia en cero

# 4. Prueba T de Student para una sola muestra (para verificar si la media de los residuos es cero)
# Esto evalúa si el sesgo global del modelo es estadísticamente nulo.
t_test_lm01 <- t.test(residuos_lm01, mu = 0)
t_test_lm02 <- t.test(residuos_lm02, mu = 0)

print(t_test_lm01)
print(t_test_lm02)

# 5. Prueba T de Student para dos muestras (muestras pareadas)
# Compara directamente si existe una diferencia significativa entre las medias de ambos residuos.
t_test_comparativo <- t.test(residuos_lm01, residuos_lm02, paired = TRUE)

print(t_test_comparativo)

# 3. Graficar los residuos para comparar visualmente sus distribuciones mediante histogramas
par(mfrow = c(1, 2)) # Divide la pantalla gráfica en 1 fila y 2 columnas

# Histograma para el Modelo Lineal Ordinario
hist(residuos_lm01, 
     main = "Residuos: Modelo Lineal", 
     xlab = "Valor del residuo", 
     ylab = "Frecuencia", 
     col  = "purple", 
     border = "white")
abline(v = 0, col = "red", lty = 2, lwd = 2) # Línea vertical de referencia en cero

# Histograma para el Modelo Lineal Robusto
hist(residuos_lm02, 
     main = "Residuos: Modelo Robusto", 
     xlab = "Valor del residuo", 
     ylab = "Frecuencia", 
     col  = "blue", 
     border = "white")
abline(v = 0, col = "red", lty = 2, lwd = 2) # Línea vertical de referencia en cero

par(mfrow = c(1, 1)) # Restaura la pantalla gráfica a su configuración normal (1x1)


# Convertirlo en Data Frame para meterle los pesos.






