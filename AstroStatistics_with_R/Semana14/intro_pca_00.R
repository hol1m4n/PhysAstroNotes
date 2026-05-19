setwd('/home/holman/Downloads/Semana14')

#Consider 100 students with Physics and Statistics grades
dat = read.table("marks.dat",head=T)
dim(dat)

names(dat)

#If we want to compare among the students which grade should be a better 
#discriminating factor? Physics or Statistics? Surely Physics, since the 
#variation is larger there. This is a common situation in data analysis 
#where the direction along which the data varies the most is of special 
#importance.

#Now suppose that the plot looks like the following. What is the best way 
#to compare the students now?
plot(dat$Phys,dat$Stat)

#Here the direction of maximum variation is like a slanted straight line. 
#This means we should take linear combination of the two grades to get the 
#best result. In this simple data set the direction of maximum variation
#is more or less clear. But for many data sets (especially high dimensional 
#ones) such visual inspection is not adequate or even possible! So we need 
#an objective method to find such a direction. Principal Component Analysis 
#(PCA) is one way to do this.
pc = princomp(~Stat+Phys,dat)

#Notice the somewhat non-intuitive syntax of the princomp function. The 
#first argument is a so-called formula object in R (we have encountered 
#this beast in the regression tutorial). In princomp the first argument 
#must start with a ~ followed by a list of the variables (separated by 
#plus signs).
pc$loading

#R has returned two principal components. (Two because we have two variables). 
#These are a unit vector at right angles to each other. You may think of PCA 
#as choosing a new coordinate system for the data, the principal components 
#being the unit vectors along the axes. The first principal component gives 
#the direction of the maximum spread of the data. The second gives the 
#direction of maximum spread perpendicular to the first direction. These two 
#directions are packed inside the matrix pc$loadings. Each column gives a 
#direction. The direction of maximum spread (the first principal component) 
#is in the first column, the next principal component in the second and so on.
pc

#to learn the amount of spread of the data along the chosen directions. Here 
#the spread along the first direction is 12.40. while that along the second 
#is much smaller 1.98. These numbers are often not of main importance, it is 
#their relative magnitude that matters.
names(pc)

#We shall not go into all these here. But one thing deserves mention: scores. 
#These are the projections of the data points along the principal components. 
#We have already mentioned that the principal components may be viewed as a 
#new reference frame. Then the scores are the coordinates of the points w.r.t. 
#this frame.
pc$scores

#Using pc results
x_fit1 = seq(0,100,1)
y_fit1 = x_fit1 * 0.695 + 15.0
y_fit2 = x_fit1 * -0.695 + 90.0

plot(dat$Phys,dat$Stat,xlim=c(0,100),ylim=c(0,100))
lines(x_fit1,y_fit1,col='blue',lwd=3)
lines(x_fit1,y_fit2,col='red',lwd=3)

x_fit1 = seq(0,100,1)
y_fit1 = x_fit1 * 0.719 + 15.0
y_fit2 = x_fit1 * -0.719 + 90.0

plot(dat$Phys,dat$Stat,xlim=c(0,100),ylim=c(0,100))
lines(x_fit1,y_fit1,col='green',lwd=3)
lines(x_fit1,y_fit2,col='magenta',lwd=3)

#It may be of some interest to know how PCA works. We shall not go into all 
#the nitty gritty details, but the basic idea is not too hard to grasp, if 
#you know eigen values and eigen vectors.

dat = read.table("marks.dat",head=T)
covar = cov(dat)

eig = eigen(covar)

val = eig$values
sqrt(val)

pc = princomp(~Stat+Phys,dat)
pc

#Now that we have seen how PCA can identify if the data cloud resides in a lower 
#dimensional space, we are ready to apply our knowledge to astronomy. We shall work 
#with the SDSS Quasar data set stored in the file SDSS_quasar.dat. First we prepare 
#the data set for analysis.

quas = read.table("SDSS_quasar.dat",head=T)
dim(quas)
names(quas)

quas = na.omit(quas)
dim(quas)

#Now we shall apply PCA.
pc = princomp(quas[,-1],scores=T)

#The scores=T option will automatically compute the projections of the data along the 
#principal component directions.
#Before looking inside pc let us make a mental note of what information we should be 
#looking for. We should look for the loadings (which are 22 mutually perpendicular 
#directions in a 22-dimensional space). Each direction is represented by a unit vector 
#with 22 components. So we have 22 times 22 = 484 numbers! Whew! We should also know 
#the spread of the data cloud along each of the 22 directions. The spread along each 
#direction is given by just a single number. So we have to just look at 22 numbers for 
#this (lot less than 484). So we shall start by looking for these 22 numbers first.
pc

#to see them. Well, some of these are much larger than the rest. To get an idea of the 
#relative magnitudes, let us plot them.
plot(pc)

#Incidentally, this plot is often called a screeplot, and R has a function with that 
#name (it produces the same output as the plot command).
screeplot(pc)

#By default, the spreads are shown as bars. Most textbooks, however, prefer to depict 
#the same information as a line diagram:
screeplot(pc,type="lines")

#The term ``scree'' refers to pebbles lying around the base of a cliff, and a screeplot 
#drawn with lines makes the analogy clear. But one should not forget that the lines do 
#not have any significance. The points are the only important things.
#We can see from the screeplot that only the first 2 components account for the bulk. 
#In other words, the 22-dimensional data cloud essentially resides in just a 2D plane! 
#Which is that plane? The answer lies in the first two columns of pc$loadings.
pc$loading[,1:2]

#These give two mutually perpendicular unit vectors defining the plane. To make sure that 
#this is indeed the case you may check as
M = pc$loading[,1:2]
t(M) %*% M #should ideally produce the 2 by 2 identity matrix

#You might like to project the entire data set onto this plane.
plot(pc$scores[,1],pc$scores[,2],pch=".")

