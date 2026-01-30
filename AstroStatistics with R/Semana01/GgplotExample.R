setwd("/home/hollman/PhysAstroNotes/AstroStatistics with R/Semana01")

library(ggplot2)

help("mtcars")

# Gráfico original
(p <- ggplot(mtcars, aes(mpg, disp)) +
    theme_classic()+
    geom_point(colour = "darkgreen"))

# Cambia la posición del eje x o y en un gráfico
p + scale_x_continuous(position = 'left')
p + scale_y_continuous(position = 'right')
p + scale_x_continuous(sec.axis = dup_axis())
p + scale_x_continuous(sec.axis = dup_axis()) +
  scale_y_continuous(sec.axis = dup_axis())

# Realiza un gráfico triangular inferior, gráficos de densidad
# en la diagonal principal y gráficas de dispersión por debajo.
require(GGally)

data(flea)
scatmat(flea, columns=2:4)

# Gráfico por especie
scatmat(flea, columns= 2:4, color="species")

#library(data.table)

mydata<-read.table("/home/hollman/PhysAstroNotes/AstroStatistics with R/Semana01/nelg2_R.dat", header = TRUE)

scatmat(mydata, columns= 3:5, color="Act")

library(dplyr)

mydata01 <- mutate_at(mydata, vars(Act, Morpho), as.factor)

scatmat(mydata01, columns= 3:5)

scatmat(mydata01, columns= 3:5, color="Act")

scatmat(mydata01, columns= 3:5, color="Morpho")

