# ANÁLISIS DE SUPERVIVENCIA

# 1. INTRODUCCIÓN
# El análisis de supervivencia lo conforman una serie de técnicas en las que 
# se realiza el seguimiento de cada sujeto durante un periodo, registrando el 
# tiempo transcurrido desde un evento inicial hasta el evento terminal (o hasta 
# el final del seguimiento si no llega a ocurrir dicho evento).

# El evento terminal se suele definir como estado ‘muerte’, de ahí lo de 
# supervivencia, ya que en sus orígenes este evento se asociaba al fallo de un 
# elemento o a la muerte de un paciente. Actualmente puede hacer referencia al 
# alta del paciente, curación de una enfermedad…

# 1.1 INSTALACIÓN DE PAQUETES
library(survival)
library(survminer)

# 1.2 BASE DE DATOS
# La base de datos fue extraída de la libreria(ISwR) del libro Statistical 
# Models Based on Counting Processes, Springer.
library(ISwR)

str(melanom)
attach(melanom)

# La base de datos melanoma contiene un total de 205 observaciones de 6 
# variables:
#  1. no. código del paciente.
# 2. status. el estado de los pacientes y el final del estudio (1=muere por 
# melanoma maligno, 2=vive al final del estudio, 3=muere por otras causas).
# 3. days. tiempo de estudio después de la operación con melanoma maligno.
# 4. ulc. superficie del melanoma vista por un microscopio si muestra 
# ulceración (1=Presente, 2=Ausencia).
# 5. thick. el grosor del tumor.
# 6. sex. género de los pacientes (1=Femenino, 2=Masculino).
head(melanom)

# 2.MÉTODO DE KAPLAN MEIER
# Conocido también como el límite del producto. La característica distintiva 
# del análisis con este método es que la proporción acumulada que sobrevive 
# se calcula para el tiempo de supervivencia individual de cada paciente y no 
# se agrupan los tiempos de supervivencia en intervalos. Por esta razón es 
# especialmente útil para estudios que utilizan un número pequeño de pacientes.

# La validez de este método descansa en dos suposiciones:
# 1. Las personas que se retiran del estudio tienen un destino parecido a las 
# que quedan.
# 2. El período de tiempo durante el cual una persona entra en el estudio no 
# tiene efecto independiente en la respuesta.

# La función de supervivencia es obtenido a través del paquete estadístico 
# survival mediante la función survfit(). Esta función en su forma más sencilla, 
# solo requiere un objeto de supervivencia creado por la función Surv().
melanom.surv <- Surv(days, status==1)
melanom.km <- survfit(melanom.surv ~ 1, data = melanom, type = "kaplan-meier")

# La estimación de la función de supervivencia se lleva a cabo con la función 
# summary().
summary(melanom.km)


# La estimación devuelve los siguientes valores:
# 1. time : Tiempo de la observación
# 2. n.risk : El número de sujetos en riesgo.
# 3. n.event : El número de sujetos que presentaron el evento.
# 4. survival : La estimación de la función de supervivencia.
# 5. std.err : La desviación estándar de la estimación.
# 6. lower y upper CI : Los intervalos de confianza para la estimación.

# 2.1 GRÁFICA DE LA CURVA DE SUPERVIVENCIA
# La curva de supervivencia estimada se gráfica con la función ggsurvplot() 
# de la paquetería survminer, está gráfica esta hecha utilizando la librería 
# ggplot2.
ggsurvplot(fit = melanom.km, data = melanom, conf.int = T, title = "Curva de Supervivencia",  xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs = "Kaplan-Meier")

# 2.2 ESTIMACIÓN DE LA MEDIA, MEDIANA Y PERCENTILES DE LOS TIEMPOS DE 
# SUPERVIVENCIA
# En R la estimación se realiza con la función print() usando como argumento 
# una estimación survfit.
print(melanom.km, print.rmean = TRUE)

# Nota: En este caso, la estimación de la supervivencia media es infinita porque 
# la curva de supervivencia no alcanza la línea del 50% antes del final del 
# estudio.
# Ahora haremos el comparativo para cuando se desee ver la relación de dos 
# curvas.
SEX.km <- survfit(Surv(days, status==1) ~ sex, data = melanom, type = "kaplan-meier") 
SEX.km

summary(SEX.km)

# 2.3 PRUEBA DE HIPÓTESIS PARA IGUALDAD DE DOS O MÁS FUNCIONES DE SUPERVIVENCIA
# Para comparar si las diferencias observadas en dos curvas de supervivencia 
# pueden ser explicadas o no por el azar, debemos realizar un test estadístico. 
# Si no hubiese observaciones censuradas la prueba no paramétrica de suma de 
# rangos de Wilcoxon podría ser apropiada para comparar dos muestras 
# independientes. Como la mayoría de las veces hay datos censurados debemos 
# utilizar otras técnicas.

# Hay diversas pruebas para comparar distribuciones de supervivencia. Aquí 
# señalaremos la prueba de logaritmo del rango (“logrank”).
# Dado dos o más grupos se tener la siguiente la siguiente prueba de hipótesis:

# H0:S1(t)=S2(t)=...=Sk(t) para todo t≤τ
# H1:Si(t0)≠Sj(t0) para al menos un par i,j y tiempo t0

# Estas pruebas de hipótesis se realizan en R utilizando la función survdiff().
survdiff(Surv(days, status==1) ~ sex, data = melanom, rho = 0)

# Su respectivo gráfico sería:
survfit(Surv(days, status==1) ~ sex, melanom, conf.type = "log-log") %>%
  ggsurvplot(title = "Supervivencia por género ", conf.int = T, legend.title = "Género", legend.labs = c("Femenino", "Masculino"))

# 3. MODELO DE RIESGOS PROPORCIONALES DE COX
# En las situaciones experimentales en las que deseamos estudiar la 
# supervivencia de un conjunto de sujetos en función de un conjunto 
# X=(X1,...,Xp) de variables predictoras, es decir, variables que pueden 
# afectar o caracterizar su supervivencia, es necesario establecer modelos 
# estadísticos capaces de analizar dichas relaciones. La construcción de 
# este tipo de modelos que depende del tiempo y de las predictoras se hace 
# a través el análisis de las función hazard asociada h(t;X).

# El modelo más habitual en esta situación es el modelo hazard proporcional 
# que separa en dos componentes la función hazard, una correspondiente al 
# tiempo de supervivencia y otra a las variables predictoras, de la forma 
# siguiente:
#  h(t;X)=h(t)exp(Xβ)

# El modelo semiparamétrico de riesgo proporcionales de Cox es realizado con 
# la función coxph() de la paquetería survival.

# 3.1 PROCESAMIENTO DE LOS DATOS
# Siempre con la misma base de melanoma plateamos un modelo en el que las 
# variables predictoras serán: sex, ulc y thick. Consideremos la variable 
# sex dado que ya pudimos ver en el análisis preliminar que se apreciaban 
# diferencias en la supervivencia por este factos.

# Para estimar correctamente el modelo de riesgos proporcionales se requiere 
# que las variables a utilizar cumplan con ciertos requisitos dependiendo su 
# tipo.

# 3.1.1 VARIABLES NOMINALES U ORDINALES
# Para el uso de variables nominales u ordinales se debe codificar cada factor 
# con un numero entero, posteriormente de leer los datos se debe indicar las 
# variables que son Nominales u Ordinales. Este procedimiento se realiza 
# utilizando la función factor().
melanom$sex <- factor(melanom$sex, labels = c("Masculino", "Femenino"))
melanom$ulc <- factor(melanom$ulc, labels = c("Ausente", "Presente"))

fit <- coxph(Surv(days, status==1) ~ melanom$sex, data = melanom)
fit

fit1 <- coxph(Surv(days, status==1) ~ melanom$ulc, data = melanom)
fit1

# 3.1.2 VARIABLES NUMÉRICAS
# Para el uso de variables numéricas, no se requiere ningún formato en 
# específico, su uso simplemente consiste en agregarla en la fórmula.
fit2 <- coxph(Surv(days, status==1) ~ thick, data = melanom)
fit2

# 3.2 ESTIMACIÓN DEL MODELO
# Como en el modelo todas la variables resultaron estadísticamente 
# significativas, entonces procederemos a elaborar el modelo final.
modelo = coxph(Surv(days, status==1) ~ melanom$sex + melanom$ulc + thick)
summary(modelo)


# Proporciona una tabla que para cada variable del modelo muestra:
# 1. coef: Coeficiente estimado de la beta.
# 2. exp(coef): Exponencial elevado al coeficiente beta estimado.
# 3. se(coef): La desviación estándar de la estimación.
# 4. z: Valor del estadístico para prueba Wald de beta igual a cero.
# 5. p: P-valor de la prueba Wald, beta igual a cero.

# Debajo de esta tabla se encuentra el resume el estadístico de la prueba 
# de razón de verosimilitud con sus respectivos grados de libertad y o valor, 
# el número de datos y observaciones del evento.

# Como la variable sex no resultó significativa procedemos nuevamente a 
# elaborar el modelo.
modelo1 = coxph(Surv(days, status==1) ~ melanom$ulc + thick)
summary(modelo1)

# Otra información importante, obtenida directamente a través de la salida 
# anterior es la estimación de los riesgos relativos (a partir de los 
# exp(coef)), con los cuales podemos decir, que la presencia de úlcera hace 
# que la muerte tenga un riesgo de 0.29 veces más el riesgo de muerte de los 
# que no tienen presencia de úlcera. En cuanto a la variable grosor del tumor 
# (thick), una persona con un tamaño de grosor determinado tiene 1.001 veces 
# el riesgo de morir en relación a una persona con tamaño de grosor menor.

# 3.3 VALIDACIÓN DE SUPUESTOS
# El modelo de Cox hace varias suposiciones. Por lo tanto, es importante 
# evaluar si un modelo de regresión de Cox ajustado describe adecuadamente 
# los datos. En concreto debemos comprobar:

# Suposición de riesgos proporcionales.
# - Existen observaciones influyentes (o valores atípicos).
# - Detectar la no linealidad de los efectos de las variables predictoras 
# en la función hazard.
# - Para verificar estas suposiciones del modelo, se utilizan diferentes 
# tipos de residuos.

# Los residuos a considerar son:
# - Residuos de Schoenfeld vs time para verificar la suposición de riesgos 
# proporcionales
# - Residuos de Schoenfeld vs time para cada predictora para evaluar la no 
# linealidad
# - Desviación residual (transformación simétrica de los residuos de 
# Martingale) para examinar observaciones influyentes

# 3.3.1 RIESGOS PROPORCIONALES
# Utilizamos la función cox.zph() para evaluar mediante tests estadísticos 
# la hipótesis de riesgos proporcionales, y la función ggcoxzph() para el 
# análisis gráfico.
riesgo = cox.zph(modelo1)
riesgo

ggcoxzph(riesgo)

# Dado que se violó el supuesto de riesgos proporcionales aplicaremos el 
# modelo de Cox estratificado que también constituye una de las maneras de 
# corregir el modelo de Cox cuando no se cumple el supuesto de riesgos 
# proporcionales para alguna de las covariables.

# En este caso suele correrse el modelo estratificando por la covariable 
# que no cumple con el supuesto de riesgo proporcional. Este procedimiento 
# permite corregir el sesgo en la estimación del parámetro que puede 
# presentarse cuando se viola el supuesto de riesgo proporcional.
modelofinal = coxph(Surv(days, status==1) ~ strata(melanom$ulc) + thick, data = melanom)
summary(modelofinal)

v=cox.zph(modelofinal)
v

ggcoxzph(v)

# La verificación del supuesto de riesgos proporcionales puede efectuarse 
# a través de un contraste de hipótesis, donde la hipótesis nula esta asociada 
# al cumplimiento del supuesto de riesgos proporcionales. Los resultados de 
# este contraste indican que no se viola el supuesto de riesgos proporcionales 
# para la covariable thick, el p-valor asociado a este contraste fue de 0.08, 
# observándose que es mayor que 0.05, con lo que no se estaría rechazando la 
# hipótesis de riesgos proporcionales.

# 3.3.2 RESIDUOS
# El análisis residual se lleva a cabo gráficamente con la función 
# ggcoxdiagnostics de la librería survminer, esta función muestra los gráficos 
# de diagnóstico de un modelo calculado con la función coxph
ggcoxdiagnostics(modelofinal, type = "deviance",
                 linear.predictions = FALSE)



