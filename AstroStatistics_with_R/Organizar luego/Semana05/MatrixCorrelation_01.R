#A correlation matrix is used to investigate the dependence between
#multiple variables at the same time. The result is a table containing the
#correlation coefficients between each variable and the others.

#There are different methods for correlation analysis : Pearson parametric
#correlation test, Spearman and Kendall rank-based correlation analysis.

#Compute correlation matrix in R
#R functions

data("mtcars")
my_data <- mtcars[, c(1,3,4,5,6,7)]

#As you may know, The R function cor() can be used to compute a correlation
#matrix. A simplified format of the function is :

cor(my_data, method = "pearson")
cor(my_data, method = "spearman")
cor(my_data, method = "kendall")

res <- cor(my_data)
round(res, 2)
