#Compute the Principal Components

#Because PCA works best with numerical data, you'll exclude the two 
#categorical variables (vs and am). You are left with a matrix of 9 
#columns and 32 rows, which you pass to the prcomp() function, 
#assigning your output to mtcars.pca. You will also set two arguments, 
#center and scale, to be TRUE. Then you can have a peek at your PCA 
#object with summary().

mtcars.pca <- prcomp(mtcars[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

summary(mtcars.pca)

#You obtain 9 principal components, which you call PC1-9. Each of these 
#explains a percentage of the total variation in the dataset. That is to 
#say: PC1 explains 63% of the total variance, which means that nearly 
#two-thirds of the information in the dataset (9 variables) can be 
#encapsulated by just that one Principal Component. PC2 explains 23% of 
#the variance. So, by knowing the position of a sample in relation to 
#just PC1 and PC2, you can get a very accurate view on where it stands in 
#relation to other samples, as just PC1 and PC2 can explain 86% of the 
#variance.

str(mtcars.pca)

#I won't describe the results here in detail, but your PCA object contains 
#the following information:

#1.- The center point ($center), scaling ($scale), standard deviation(sdev) 
#of each principal component
#2.- The relationship (correlation or anticorrelation, etc) between the 
#initial variables and the principal components ($rotation)
#3.- The values of each sample in terms of the principal components ($x)

#Plotting PCA
#Now it's time to plot your PCA. You will make a biplot, which includes both 
#the position of each sample in terms of PC1 and PC2 and also will show you 
#how the initial variables map onto this. You will use the ggbiplot package, 
#which offers a user-friendly and pretty function to plot biplots. A biplot 
#is a type of plot that will allow you to visualize how the samples relate to 
#one another in our PCA (which samples are similar and which are different) 
#and will simultaneously reveal how each variable contributes to each 
#principal component.

library(devtools)
#install_github("vqv/ggbiplot")

library(ggbiplot)

ggbiplot(mtcars.pca)

#The axes are seen as arrows originating from the center point. Here, you see 
#that the variables hp, cyl, and disp all contribute to PC1, with higher values 
#in those variables moving the samples to the right on this plot. This lets you 
#see how the data points relate to the axes, but it's not very informative 
#without knowing which point corresponds to which sample (car).

#You'll provide an argument to ggbiplot: let's give it the rownames of mtcars 
#as labels. This will name each point with the name of the car in question:

ggbiplot(mtcars.pca, labels=rownames(mtcars))

#Interpreting the Results
#Maybe if you look at the origin of each of the cars. You'll put them into one 
#of three categories (cartegories?), one each for the US, Japanese and European 
#cars. You make a list for this info, then pass it to the groups argument of 
#ggbiplot. You'll also set the ellipse argument to be TRUE, which will draw an 
#ellipse around each group.

mtcars.country <- c(rep("Japan", 3), rep("US",4), rep("Europe", 7),rep("US",3), "Europe", rep("Japan", 3), rep("US",4), rep("Europe", 3), "US", rep("Europe", 3))

ggbiplot(mtcars.pca,ellipse=TRUE,  labels=rownames(mtcars), groups=mtcars.country)

#Now you see something interesting: the American cars form a distinct cluster 
#to the right. Looking at the axes, you see that the American cars are 
#characterized by high values for cyl, disp, and wt. Japanese cars, on the 
#other hand, are characterized by high mpg. European cars are somewhat in the 
#middle and less tightly clustered than either group.

#Of course, you have many principal components available, each of which map 
#differently to the original variables. You can also ask ggbiplot to plot these 
#other components, by using the choices argument.

#Let's have a look at PC3 and PC4:

ggbiplot(mtcars.pca,ellipse=TRUE,choices=c(3,4),   labels=rownames(mtcars), groups=mtcars.country)

#You don't see much here, but this isn't too surprising. PC3 and PC4 explain 
#very small percentages of the total variation, so it would be surprising if 
#you found that they were very informative and separated the groups or 
#revealed apparent patterns.

#Let's take a moment to recap: having performed a PCA using the mtcars dataset, 
#we can see a clear separation between American and Japanese cars along a 
#principal component that is closely correlated to cyl, disp, wt, and mpg. 
#This provides us with some clues for future analyses; if we were to try to 
#build a classification model to identify the origin of a car, these variables 
#might be useful.

#Graphical Parameters with ggbiplot
#There are also some other variables you can play with to alter your biplots. 
#You can add a circle to the center of the dataset (circle argument):

ggbiplot(mtcars.pca,ellipse=TRUE,circle=TRUE, labels=rownames(mtcars), groups=mtcars.country)

#You can also scale the samples (obs.scale) and the variables (var.scale):
  
ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country)

#You can also remove the arrows altogether, using var.axes.

ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,var.axes=FALSE,   labels=rownames(mtcars), groups=mtcars.country)

#Customize ggbiplot
#As ggbiplot is based on the ggplot function, you can use the same set of 
#graphical parameters to alter your biplots as you would for any ggplot. Here, you're going to:

#1.- Specify the colours to use for the groups with scale_colour_manual()
#2.- Add a title with ggtitle()
#3.- Specify the minimal() theme
#4.- Move the legend with theme()

ggbiplot(mtcars.pca,ellipse=TRUE,obs.scale = 1, var.scale = 1,  labels=rownames(mtcars), groups=mtcars.country) +
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "dark blue"))+
  ggtitle("PCA of mtcars dataset")+
  theme_minimal()+
  theme(legend.position = "bottom")

#Adding a New Sample
#Okay, so let's say you want to add a new sample to your dataset. This is a 
#very special car, with stats unlike any other. It's super-powerful, has a 
#60-cylinder engine, amazing fuel economy, no gears and is very light. It's 
#a "spacecar", from Jupiter.

#Can you add it to your existing dataset and see where it places in relation 
#to the other cars?

#You will add it to mtcars, creating mtcarsplus, then repeat your analysis. 
#You might expect to be able to see which region's cars it is most like.

spacecar <- c(1000,60,50,500,0,0.5,2.5,0,1,0,0)

mtcarsplus <- rbind(mtcars, spacecar)
mtcars.countryplus <- c(mtcars.country, "Jupiter")

mtcarsplus.pca <- prcomp(mtcarsplus[,c(1:7,10,11)], center = TRUE,scale. = TRUE)

ggbiplot(mtcarsplus.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = FALSE, var.axes=TRUE, labels=c(rownames(mtcars), "spacecar"), groups=mtcars.countryplus)+
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "violet", "dark blue"))+
  ggtitle("PCA of mtcars dataset, with extra sample added")+
  theme_minimal()+
  theme(legend.position = "bottom")

#But that would be a naive assumption! The shape of the PCA has changed 
#drastically, with the addition of this sample. When you consider this 
#result in a bit more detail, it actually makes perfect sense. In the 
#original dataset, you had strong correlations between certain variables 
#(for example, cyl and mpg), which contributed to PC1, separating your 
#groups from one another along this axis. However, when you perform the 
#PCA with the extra sample, the same correlations are not present, which 
#warps the whole dataset. In this case, the effect is particularly strong 
#because your extra sample is an extreme outlier in multiple respects.

#If you want to see how the new sample compares to the groups produced by 
#the initial PCA, you need to project it onto that PCA.

#Project a New Sample onto the Original PCA
#What this means is that the principal components are defined without 
#relation to your spacecar sample, then you compute where spacecar is 
#placed in relation to the other samples by applying the transformations 
#that your PCA has produced. You can think of this as, instead of getting 
#the mean of all the samples and allowing spacecar to skew this mean, you 
#get the mean of the rest of the samples and look at spacecar in relation 
#to this.

#What this means is that you simply scale the values for spacecar in 
#relation to the PCA's center (mtcars.pca$center). Then you apply the 
#rotation of the PCA matrix to the spacecar sample. Then you can rbind() 
#the projected values for spacecar to the rest of the pca$x matrix and 
#pass this to ggbiplot as before:

s.sc <- scale(t(spacecar[c(1:7,10,11)]), center= mtcars.pca$center)
s.pred <- s.sc %*% mtcars.pca$rotation

mtcars.plusproj.pca <- mtcars.pca
mtcars.plusproj.pca$x <- rbind(mtcars.plusproj.pca$x, s.pred)

ggbiplot(mtcars.plusproj.pca, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = FALSE, var.axes=TRUE, labels=c(rownames(mtcars), "spacecar"), groups=mtcars.countryplus)+
  scale_colour_manual(name="Origin", values= c("forest green", "red3", "violet", "dark blue"))+
  ggtitle("PCA of mtcars dataset, with extra sample projected")+
  theme_minimal()+
  theme(legend.position = "bottom")

#This result is drastically different. Note that all the other samples 
#are back in their initial positions, while spacecar is placed somewhat 
#near the middle. Your extra sample is no longer skewing the overall 
#distribution, but it can't be assigned to a particular group.

#But which is better, the projection or the recomputation of the PCA?

#It depends somewhat on the question that you are trying to answer; the 
#recomputation shows that spacecar is an outlier, the projection tells 
#you that you can't place it in one of the existing groups. Performing 
#both approaches is often useful when doing exploratory data analysis by 
#PCA. This type of exploratory analysis is often a good starting point 
#before you dive more deeply into a dataset. Your PCAs tell you which 
#variables separate American cars from others and that spacecar is an 
#outlier in our dataset. A possible next step would be to see if these 
#relationships hold true for other cars or to see how cars cluster by 
#marque or by type (sports cars, 4WDs, etc).

