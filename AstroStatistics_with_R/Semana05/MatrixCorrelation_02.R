#Correlation matrix with significance levels (p-value)
library("Hmisc")

# The function rcorr() [in Hmisc package] can be used to 
# compute the significance levels for pearson and spearman 
# correlations. It returns both the correlation coefficients 
# and the p-value of the correlation for all possible pairs 
#of columns in the data table.
res2 <- rcorr(as.matrix(my_data))
res2

# Extract the correlation coefficients
res2$r

# Extract p-values
res2$P

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

res2<-rcorr(as.matrix(mtcars[,1:7]))

# This section provides a simple function for formatting a 
# correlation matrix into a table with 4 columns containing :
# Column 1 : row names (variable 1 for the correlation test)
# Column 2 : column names (variable 2 for the correlation test)
# Column 3 : the correlation coefficients
# Column 4 : the p-values of the correlations
flattenCorrMatrix(res2$r, res2$P)

# The R function symnum() replaces correlation coefficients 
# by symbols according to the level of the correlation. It takes 
# the correlation matrix as an argument :
symnum(res, abbr.colnames = FALSE)

# Draw a correlogram
library(corrplot)

# The function corrplot(), in the package of the same name, 
# creates a graphical display of a correlation matrix, 
# highlighting the most correlated variables in a data table.
# In this plot, correlation coefficients are colored according 
# to the value. Correlation matrix can be also reordered 
# according to the degree of association between variables.
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.01, insig = "blank")

# Draw scatter plots
library("PerformanceAnalytics")

my_data <- mtcars[, c(1,3,4,5,6,7)]

# The function chart.Correlation()[ in the package 
# PerformanceAnalytics], can be used to display a chart of a 
# correlation matrix.
# - The distribution of each variable is shown on the diagonal.
# - On the bottom of the diagonal : the bivariate scatter plots 
# with a fitted line are displayed
# - On the top of the diagonal : the value of the correlation 
# plus the significance level as stars
# - Each significance level is associated to a symbol : 
# p-values(0, 0.001, 0.01, 0.05, 0.1, 1) <=> symbols(“***”, “**”, “*”, “.”, " “)
chart.Correlation(my_data, histogram=TRUE, pch=19)

# Use heatmap
# Get some colors
col<- colorRampPalette(c("blue", "white", "red"))(20)

# x : the correlation matrix to be plotted
# col : color palettes
# symm : logical indicating if x should be treated symmetrically; 
# can only be true when x is a square matrix.
heatmap(x = res, col = col, symm = TRUE)
