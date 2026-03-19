mydata = read.table("~/Downloads/Semana06/nelg2_R.dat",h=T)
attach(mydata)

library(multcomp)
library(sandwich)
library(gplots)

boxplot(Bulge ~ Act,outline=F,notch=T)
plotmeans(Bulge ~ Act,add=T,ty="p",xaxt="n")

library(dplyr)

mydata <- mutate_at(mydata,vars(Act,Morpho),as.factor)

fic1=aov(Bulge ~ Act,data=mydata)
fic1_glht = glht(fic1,mcp(Act="Tukey"),vcov=vcovHC)
summary(fic1_glht)

par(mar=c(5,7,3,3))
plot(confint(fic1_glht))

fic1=aov(Bulge ~ Act,data=mydata)
fic1_glht = glht(fic1,mcp(Act="Dunnett"),vcov=vcovHC)
summary(fic1_glht)

par(mar=c(5,7,3,3))
plot(confint(fic1_glht))



fic1=aov(Bulge ~ Morpho,data=mydata)
fic1_glht = glht(fic1,mcp(Morpho="Tukey"),vcov=vcovHC)
summary(fic1_glht)

par(mar=c(5,7,3,3))
plot(confint(fic1_glht))



# Ahora para MB, copiar todo lo de arriba

# Estudiar luego ANOVA y ANCOVA (concepto nuevo).



boxplot(MB ~ Act,outline=F,notch=T)
plotmeans(MB ~ Act,add=T,ty="p",xaxt="n")

mydata <- mutate_at(mydata,vars(Act,Morpho),as.factor)

fic1=aov(MB ~ Act,data=mydata)
fic1_glht = glht(fic1,mcp(Act="Tukey"),vcov=vcovHC)
summary(fic1_glht)

par(mar=c(5,7,3,3))
plot(confint(fic1_glht))


fic1=aov(MB ~ Morpho,data=mydata)
fic1_glht = glht(fic1,mcp(Morpho="Tukey"),vcov=vcovHC)
summary(fic1_glht)

par(mar=c(5,7,3,3))
plot(confint(fic1_glht))


# Tarea hacer ANOVA entre el indice de concetracion IC y la morfologia.










