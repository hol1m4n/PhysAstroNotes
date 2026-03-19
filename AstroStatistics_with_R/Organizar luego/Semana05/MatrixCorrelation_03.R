# Correlogram in R
# required packages
library(corrplot)

head(mtcars)

# correlation matrix
M <- cor(mtcars)
head(round(M,2))

# visualizing correlogram
# as circle
corrplot(M, method="circle")

# as pie
corrplot(M, method="pie")

# as colour
corrplot(M, method="color")

# as number
corrplot(M, method="number")

