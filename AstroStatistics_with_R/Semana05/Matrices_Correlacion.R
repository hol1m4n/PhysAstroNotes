library(corrplot)
#install.packages("Hmisc")
library("Hmisc")
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)


nombres_columnas <- c("z", "LogMbh", "LogL5100", "LogBolEdd", "W2W3", "T", "ba", 
                      "beta", "FWHMO3Core", "RA.degree", "Dec.degree", "MJD", "PLATE", "FIBER")

seleccion_columnas <- c("LogMbh", "LogL5100", "LogBolEdd", "W2W3", "T" ,"beta")


datos_completos <- read.table("Table_Sy1Broad.dat", 
                    header = FALSE, 
                    skip = 1,          
                    sep = "",   
                    col.names = nombres_columnas)

datos1 <- datos_completos[, seleccion_columnas]

M1 <- cor(datos1, method = "spearman", use = "complete.obs")
head(round(M1,2))

corrplot(M1, method="circle")
corrplot(M1, method="pie")
corrplot(M1, method="color")
corrplot(M1, method="number")

corrplot(M1, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
chart.Correlation(datos1, histogram=TRUE, pch=19)








datos_completos <- read.table("Table_Sy1Broad_wind.dat", 
                              header = FALSE, 
                              skip = 1,          
                              sep = "",   
                              col.names = nombres_columnas)

datos2 <- datos_completos[, seleccion_columnas]

M2 <- cor(datos2, method = "spearman", use = "complete.obs")
head(round(M2,2))

corrplot(M2, method="circle")
corrplot(M2, method="pie")
corrplot(M2, method="color")
corrplot(M2, method="number")

corrplot(M2, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
chart.Correlation(datos2, histogram=TRUE, pch=19)








datos_completos <- read.table("Table_Sy1Narrow.dat", 
                              header = FALSE, 
                              skip = 1,          
                              sep = "",   
                              col.names = nombres_columnas)

datos3 <- datos_completos[, seleccion_columnas]

M3 <- cor(datos3, method = "spearman", use = "complete.obs")
head(round(M3,2))

corrplot(M3, method="circle")
corrplot(M3, method="pie")
corrplot(M3, method="color")
corrplot(M3, method="number")

corrplot(M3, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
chart.Correlation(datos3, histogram=TRUE, pch=19)



datos_completos <- read.table("Table_Sy1Narrow_wind.dat", 
                              header = FALSE, 
                              skip = 1,          
                              sep = "",   
                              col.names = nombres_columnas)

datos4 <- datos_completos[, seleccion_columnas]

M4 <- cor(datos4, method = "spearman", use = "complete.obs")
head(round(M4,2))

corrplot(M4, method="circle")
corrplot(M4, method="pie")
corrplot(M4, method="color")
corrplot(M4, method="number")

corrplot(M4, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
chart.Correlation(datos4, histogram=TRUE, pch=19)




