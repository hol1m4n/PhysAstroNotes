# install.packages("ggplot2")
# install.packages("ggExtra")
library(ggplot2)
library(ggExtra)

# Save the scatter plot in a variable
p <- ggplot(cars, aes(x = speed, y = dist)) + geom_point()

# Plot the scatter plot with marginal histograms
ggMarginal(p, type = "histogram")

# Horizontal marginal histogram
ggMarginal(p, type = "histogram", margins = "x")

# Vertical marginal histogram
ggMarginal(p, type = "histogram", margins = "y")

# Changing the relative size
ggMarginal(p, type = "histogram", size = 3)

# Bin width customization
ggMarginal(p, type = "histogram", binwidth = 4)

# Densigram
ggMarginal(p, type = "densigram")

# Changing the fill color
ggMarginal(p, type = "histogram", fill = 4)

# Fill and border color of the marginal histograms
ggMarginal(p, type = "histogram", fill = "white", col = 4)

# Arguments for each marginal histogram
ggMarginal(p, type = "histogram", xparams = list(fill = 4), yparams = list(fill = 3))

# Sample grouping variable
cars$group <- c(rep("A", 25), rep("B", 25))

# Save the scatter plot in a variable
p <- ggplot(cars, aes(x = speed, y = dist, color = group)) + geom_point()

# Marginal histograms by group
ggMarginal(p, type = "histogram", groupColour = TRUE, groupFill = TRUE)

# Marginal density by group
ggMarginal(p, groupColour = TRUE, groupFill = TRUE)
