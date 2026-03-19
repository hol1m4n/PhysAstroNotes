
# Tarea 5

library(dplyr)

library(data.table)

setwd('~/PhysAstroNotes/AstroStatistics_with_R/Semana05/')

Seyfert_T = read.table("Table_Sy1Broad.dat", header = TRUE)


Sey_clean <- Seyfert_T[, c("z","LogMbh","LogL5100","LogBolEdd","W2W3","T","beta")]

cor(Sey_clean, method = "spearman")


res <- cor(Sey_clean, method = "spearman")
round(res, 2)





