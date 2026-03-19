mtcars$cyl <- as.factor(mtcars$cyl)

head(mtcars)

library(ggplot2)

#geom_point(size, color, shape)

# Aesthetic mappings describe how variables in the data
# are mapped to visual properties (aesthetics) of geoms. 

# Basic scatter plot
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(size=2, shape=23)

# Change the point size
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(aes(size=qsec))

# Label points in the scatter plot
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() + 
  geom_text(label=rownames(mtcars))

# Add the regression line
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_smooth(method=lm)

# Remove the confidence interval
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE)

# Add the regression line with 95% level of conficende interval
# Default value is 0.95
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point() +
  geom_smooth(method=lm, level=0.90)

# Loess method
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point()+
  geom_smooth()

# Change the point colors and shapes
# Change the line type and color
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(shape=18, color="blue") +
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")

# Change the confidence interval fill color
ggplot(mtcars, aes(x=wt, y=mpg)) +
  geom_point(shape=18, color="blue") +
  geom_smooth(method=lm,  linetype="dashed",
              color="darkred", fill="blue")

# Change point shapes by the levels of cyl
ggplot(mtcars, aes(x=wt, y=mpg, shape=cyl)) +
  geom_point()

# Change point shapes and colors
ggplot(mtcars, aes(x=wt, y=mpg, shape=cyl, color=cyl)) +
  geom_point()

# Change point shapes, colors and sizes
ggplot(mtcars, aes(x=wt, y=mpg, shape=cyl, color=cyl, size=cyl)) +
  geom_point()

# Add regression lines
ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl)) +
  geom_point() +
  geom_smooth(method=lm)

# Remove confidence intervals
# Extend the regression lines
ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl)) +
  geom_point() +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl)) +
  geom_point() +
  geom_smooth(method=lm, aes(fill=cyl))

# Change point shapes and colors manually
ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 17))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  theme(legend.position="top")

# Change the point sizes manually
ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl))+
  geom_point(aes(size=cyl)) + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)+
  scale_shape_manual(values=c(3, 16, 17))+ 
  scale_color_manual(values=c('#999999','#E69F00', '#56B4E9'))+
  scale_size_manual(values=c(2,3,4))+
  theme(legend.position="top")


library(ggplot2)
attach(mtcars)

View(mtcars)


color = c("blue","orange","purple")

ggplot(mtcars,aes(cyl)) + geom_bar(fill=color) +
labs(title="Número de cilindros",x="Cilindros",y="Frecuencias") +
theme_dark()
