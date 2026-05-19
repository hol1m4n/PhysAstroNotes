library(tidyverse)

#load data
data("USArrests")

#view first six rows of data
head(USArrests)

#calculate principal components
results <- prcomp(USArrests, scale = TRUE)

#reverse the signs
results$rotation <- -1*results$rotation

#display principal components
results$rotation

#reverse the signs of the scores
results$x <- -1*results$x

#display the first six scores
head(results$x)

biplot(results, scale = 0)

plot(USArrests$Murder,USArrests$Assault)

plot(USArrests$Murder,USArrests$UrbanPop)

plot(USArrests$Rape,USArrests$UrbanPop)

#display states with highest murder rates in original dataset
head(USArrests[order(-USArrests$Murder),])

#calculate total variance explained by each principal component
results$sdev^2 / sum(results$sdev^2)

#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:4), var_explained) +
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)

#load pls package
library(pls)

#make this example reproducible
set.seed(1)

#fit PCR model
model <- pcr(UrbanPop~Murder+Assault+Rape, data=USArrests, scale=TRUE, validation="CV")

#view summary of model fitting
summary(model)

#visualize cross-validation plots
validationplot(model)
validationplot(model, val.type="MSEP")
validationplot(model, val.type="R2")

#define training and testing sets
train <- USArrests[1:25, c("UrbanPop", "Murder", "Assault", "Rape")]
y_test <- USArrests[26:nrow(USArrests), c("UrbanPop")]
test <- USArrests[26:nrow(USArrests), c("Murder", "Assault", "Rape")]
    
#use model to make predictions on a test set
model01 <- pcr(UrbanPop~Murder+Assault+Rape, data=train, scale=TRUE, validation="CV")
pcr_pred <- predict(model01, test, ncomp=2)

#calculate RMSE
sqrt(mean((pcr_pred - y_test)^2))

