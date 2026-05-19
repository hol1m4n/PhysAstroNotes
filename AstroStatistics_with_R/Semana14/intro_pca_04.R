#PCA is a powerful technique that reduces data dimensions, it
#1.- Makes sense of the big data.
#2.- Gives an overall shape of the data.
#3.- Identifies which samples are similar and which are different.

setwd("/home/holman/Semana14")

#Here I will not go into the theory behind PCA, instead, I will focus 
#on how to do PCA and how to read the PCA plot. As example I will be 
#using data from this paper “Single-Cell RNA-Seq Reveals Lineage and 
#X Chromosome Dynamics in Human Preimplantation Embryos“. In the 
#preimplantation embryos cells transform from being one type to become 
#three types at day 5.

#I will be using the data from Table S2. They listed around 100 specific 
#gene for each cell type.

#First you need to download the table and prepare it as shown above and 
#save as a CSV format (data.csv). Then you can upload it into R by using 
#the command below:

data <- read.csv("data_table_S2.csv", row.names = 1)

#Now we need to install and load two R package which will allow us to do 
#PCA in R
#intall

#install.packages(c("factoextra", "FactoMineR"))

#load
library("factoextra")
library("FactoMineR")

#We will use the PCA() function. PCA() function first standardize the 
#values then creates a new principal component table where first principal
#component (PC1) corresponds to the directions with the maximum amount of 
#variation in the data set. The second principal component (PC2) corresponds 
#to the directions with the second maximum amount of variation in the data 
#set and third, forth, etc..

pca.data <- PCA(data[,-1], scale.unit = TRUE, graph = FALSE)

#In the command above we used this argument data[,-1] to omit the first 
#column from our analysis as it has no numeric values. scale.unit = TRUE 
#is an argument to standardize the values. For more information bout the 
#arguments of PCA() function, you can visit the R documentation.

#To make sure that most of the data will be presented in the PCA plot, we 
#need to use the fviz_eig() function. We will be using the table we created 
#with PCA() function; pca.data

fviz_eig(pca.data, addlabels = TRUE, ylim = c(0, 70))

#As we can see below, the first two principal components explain 81.6% of 
#the variation. Here there no one rule you can rely on to make sure your 
#model is acceptable but it as acceptable if the first three principal 
#components explained around 70%.

#To understand the correlation between the samples and how they are well 
#represented by our model we can use fviz_pca_var() function to draw a 
#variable correlation plot by using the command below

fviz_pca_var(pca.data, col.var = "cos2",
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"),
             repel = TRUE)

#We can see below the EPI cell type are next to each other, this means 
#they are correlated to each other. Here we do not have any negative 
#correlation between the variables but if there was the arrow will be on 
#the opposite sides. One last thing, if the arrow is close to the circle 
#(long), it means the variable is well represented. 

#When doing a PCA plot we have the option to plot the cell types (sample) 
#or the gene expression (the values). It gives us the opportunity to look 
#at the data from different angles which could enable us to find a pattern 
#or a marker.

#Lets start by plotting the cell types first. To do that we need to used 
#the PCA() function again and use t() function to flip our table, so we 
#can put the cell types as rows.

pca.data <- PCA(t(data[,-1]), scale.unit = TRUE, graph = FALSE)

#Then we will use the viz_pca_ind() function for the visualization as 
#shown below

fviz_pca_ind(pca.data, col.ind = "cos2", 
             gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
             repel = TRUE)

#The PCA plot below shows that Day 3, 4 and Pre_day 5 has no correlation 
#with the day 5, 6 and 7. This is because the table we used only reported 
#highly 100 expressed gene in PE, TE and EPI. We can see each type of 
#cell (PE, TE and EPI) starting from day 5 grouped together which means 
#they have the same gene expression profile.

#To add labels to the PCA plot we can use ggpubr package. First we need 
#to install and load the package 

#install
devtools::install_github("kassambara/ggpubr")
#load
library(ggpubr)

#then we need to assign the previous command to a

a <- fviz_pca_ind(pca.data, col.ind = "cos2", 
                  gradient.cols = c("#FFCC00", "#CC9933", "#660033", "#330033"), 
                  repel = TRUE)

#Now we can use ggpar() function to add labels

ggpar(a,
      title = "Principal Component Analysis",
      xlab = "PC1", ylab = "PC2",
      legend.title = "Cos2", legend.position = "top",
      ggtheme = theme_minimal())

#Now lets plot the gene instead of the cell types. We will use the 
#PCA() function

pca.data <- PCA(data[,-1], scale.unit = TRUE,ncp = 2, graph = FALSE)

#To color the gene in the PCA plot we will be using the first column 
#(Lineage), it divided the cells into three groups. First we need to 
#convert the column to a factor by the following command

data$Lineage <- as.factor(data$Lineage)

#For the coloring palette we will use the commands below. Here I have 
#3 groups that’s why I wrote 3 in the commands below. If you have 4 or 
#5 groups make sure you change the colors from 3 to 4 or 5. 

install.packages("RColorBrewer")
library(RColorBrewer)
nb.cols <- 3
mycolors <- colorRampPalette(brewer.pal(3, "Set1"))(nb.cols)

#We will use fviz_pca_ind() function to create the PCA plot and assign 
#it to a as we did previously.

#The second argument below; col.ind = data$Lineage is color indicator, 
#here we used the Lineage column. The third argument is the colors we 
#will using (mycolors is what create above). The last argument, 
#addEllipses = TRUE is adding an oval shape around each group.

a <- fviz_pca_ind(pca.data, col.ind = data$Lineage,
                  palette = mycolors, addEllipses = TRUE)


#Then we will use the ggpar() function to add labels

ggpar(a,
      title = "Principal Component Analysis",
      xlab = "PC1", ylab = "PC2",
      legend.title = "Cell type", legend.position = "top",
      ggtheme = theme_minimal())

#From the plot above, we can see that there are some genes are associated 
#with a specific type of cell. These genes could act as marker to identify 
#these cell types. In TE cells the KRT18, KRT8 and S100A16 are highly 
#expresses compare to the other genes. In EPI cells the DPPA5, IFITM1 , 
#MT1X and UPP1 are highly expressed compare to other genes. And in PE cells 
#only APOA1 is highly expressed compare to the other genes.

#To understand any PCA plot, one should know that similar samples cluster 
#together and different samples cluster away from each others. As shown 
#below we can also understand if the samples are negatively or positively 
#correlate with each others.

