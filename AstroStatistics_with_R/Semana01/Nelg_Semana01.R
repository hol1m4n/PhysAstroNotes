setwd("/home/hollman/PhysAstroNotes/AstroStatistics with R/Semana01")

require(GGally)

data(flea)

scatmat(flea, columns=2:4)

scatmat(flea, columns= 2:4, color="species")

mydata<-read.table("nelg2_R.dat", header = TRUE)

require(GGally)

scatmat(mydata, columns= 3:5, color="Act")



library(dplyr)

mydata<-read.table("nelg1_R.dat", header = TRUE)

require(GGally)

mydata01 <- mutate_at(mydata, vars(Act, Morp), as.factor)

scatmat(mydata01, columns= 7:9)

scatmat(mydata01, columns= 7:9, color="Act")

scatmat(mydata01, columns= 7:9, color="Morp")

result01 = mydata01$Morp == "S0" | mydata01$Morp == "S0a"
result02 = mydata01$Morp == "Sa" | mydata01$Morp == "Sab" | mydata01$Morp == "Sb"
result03 = mydata01$Morp == "Sc" | mydata01$Morp == "Sd" | mydata01$Morp == "Sm"

mydata01$IC[result01]
mydata01$IC[result02]
mydata01$IC[result03]

summary(mydata01$IC[result01])
summary(mydata01$IC[result02])
summary(mydata01$IC[result03])


mydata01$groups <- factor(
  ifelse(mydata01$Morp == "S0"  | mydata01$Morp == "S0a", "early",
         ifelse(mydata01$Morp == "Sa"  | mydata01$Morp == "Sab" | mydata01$Morp == "Sb",
                "early spiral",
                "late spiral"))
)


table(mydata01$groups)

library(GGally)
scatmat(mydata01, columns = 7:9, color = "groups")


