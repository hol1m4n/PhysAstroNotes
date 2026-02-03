#Comparing data is an important part of data science. The QQ plot is
#an excellent way of making and showing such comparisons. These
#comparisons are usually made to look for relationships between data
#sets and comparing a real data set to a mathematical model of the
#system being studied. This type of probability plot is great for
#testing sample data, residuals, finding a theoretical quantile, or
#finding a specific data point. We’re going to share how to make a
#qq plot in r.

#A QQ plot; also called a Quantile Quantile plot; is a scatter plot
#that compares two sets of data. A common use of QQ plots is checking
#the normality of data.

# Set seed for reproducibility
set.seed(500)

# Create random normally distributed values
x <- rnorm(1200)

# QQplot of normally distributed values
qqnorm(x)

# Add qqline to plot
qqline(x, col = "darkgreen", lwd=4)

# Set seed for reproducibility
set.seed(500)

# Random values according to logistic distribution
# QQplot of logistic distribution
y <- rlogis(800)

# QQplot of normally distributed values
qqnorm(y)

# Add qqline to plot
qqline(y, col = "darkgreen", lwd=4)

# how to make a QQ plot in R for two samples
x = rnorm(100, 50, 25)
y = rnorm(100, 50, 25)

# qqplot function in r package
qqplot(x, y, xlab = "test x", ylab = "test y", main = "Q-Q Plot")
