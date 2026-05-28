# With roots dating back to at least 1662 when John Graunt, a London merchant,
# published an extensive set of inferences based on mortality records, survival 
# analysis is one of the oldest subfields of Statistics. Basic life-table 
# methods, including techniques for dealing with censored data, were discovered 
# before 1700, and in the early eighteenth century, the old masters - 
# de Moivre working on annuities, and Daniel Bernoulli studying competing risks 
# for the analysis of smallpox inoculation - developed the modern foundations of 
# the field. Today, survival analysis models are important in Engineering, 
# Insurance, Marketing, Medicine, and many more application areas. So, it is not 
# surprising that R should be rich in survival analysis functions. CRAN’s 
# Survival Analysis Task View, a curated list of the best relevant R survival 
# analysis packages and functions, is indeed formidable. We all owe a great deal 
# of gratitude to Arthur Allignol and Aurielien Latouche, the task view 
# maintainers.

# Looking at the Task View on a small screen, however, is a bit like standing 
# too close to a brick wall - left-right, up-down, bricks all around. It is a 
# fantastic edifice that gives some idea of the significant contributions R 
# developers have made both to the theory and practice of Survival Analysis. 
# As well-organized as it is, however, I imagine that even survival analysis 
# experts need some time to find their way around this task view. Newcomers - 
# people either new to R or new to survival analysis or both - must find it 
# overwhelming. So, it is with newcomers in mind that I offer the following 
# narrow trajectory through the task view that relies on just a few packages: 
# survival, ggplot2, ggfortify, and ranger

# The survival package is the cornerstone of the entire R survival analysis 
# edifice. Not only is the package itself rich in features, but the object 
# created by the Surv() function, which contains failure time and censoring 
# information, is the basic survival analysis data structure in R. Dr. Terry 
# Therneau, the package author, began working on the survival package in 1986. 
# The first public release, in late 1989, used the Statlib service hosted by 
# Carnegie Mellon University. Thereafter, the package was incorporated directly 
# into Splus, and subsequently into R.

# ggfortify enables producing handsome, one-line survival plots with 
# ggplot2::autoplot.

# ranger might be the surprise in my very short list of survival packages.
# The ranger() function is well-known for being a fast implementation of the 
# Random Forests algorithm for building ensembles of classification and 
# regression trees. But ranger() also works with survival data. Benchmarks 
# indicate that ranger() is suitable for building time-to-event models with 
# the large, high-dimensional data sets important to internet marketing 
# applications. Since ranger() uses standard Surv() survival objects, it’s an 
# ideal tool for getting acquainted with survival analysis in this 
# machine-learning age.

# Load the data
# This first block of code loads the required packages, along with the veteran 
# dataset from the survival package that contains data from a two-treatment, 
# randomized trial for lung cancer.
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)

#------------
data(veteran)
head(veteran)

# The variables in veteran are: * trt: 1=standard 2=test * celltype: 
# 1=squamous, 2=small cell, 3=adeno, 4=large * time: survival time in 
# days * status: censoring status * karno: Karnofsky performance score 
# (100=good) * diagtime: months from diagnosis to randomization * age: 
# in years * prior: prior therapy 0=no, 10=yes

# Kaplan Meier Analysis
# The first thing to do is to use Surv() to build the standard survival 
# object. The variable time records survival time; status indicates whether 
# the patient’s death was observed (status = 1) or that survival time was 
# censored (status = 0). Note that a “+” after the time in the print out of 
# km indicates censoring.
# Kaplan Meier Survival Curve
km <- with(veteran, Surv(time, status))
head(km,80)

# To begin our analysis, we use the formula Surv(futime, status) ~ 1 and the 
# survfit() function to produce the Kaplan-Meier estimates of the probability 
# of survival over time. The times parameter of the summary() function gives 
# some control over which times to print. Here, it is set to print the 
# estimates for 1, 30, 60 and 90 days, and then every 90 days thereafter. This 
# is the simplest possible model. It only takes three lines of R code to fit 
# it, and produce numerical and graphical summaries.
km_fit <- survfit(Surv(time, status) ~ 1, data=veteran)
summary(km_fit, times = c(1,30,60,90*(1:10)))

plot(km_fit, xlab="Days", main = 'Kaplan Meyer Plot')

# base graphics is always ready
autoplot(km_fit)

#Next, we look at survival curves by treatment.
km_trt_fit <- survfit(Surv(time, status) ~ trt, data=veteran)
autoplot(km_trt_fit)

# And, to show one more small exploratory plot, I’ll do just a little data 
# munging to look at survival by age. First, I create a new data frame with 
# a categorical variable AG that has values LT60 and GT60, which respectively 
# describe veterans younger and older than sixty. While I am at it, I make 
# trt and prior into factor variables. But note, survfit() and npsurv() 
# worked just fine without this refinement.
vet <- mutate(veteran, AG = ifelse((age < 60), "LT60", "OV60"),
              AG = factor(AG),
              trt = factor(trt,labels=c("standard","test")),
              prior = factor(prior,labels=c("N0","Yes")))


km_AG_fit <- survfit(Surv(time, status) ~ AG, data=vet)

autoplot(km_AG_fit)

# Although the two curves appear to overlap in the first fifty days, younger 
# patients clearly have a better chance of surviving more than a year.

# Cox Proportional Hazards Model
# Next, I’ll fit a Cox Proportional Hazards Model that makes use of all of 
# the covariates in the data set.
# Fit Cox Model
cox <- coxph(Surv(time, status) ~ trt + celltype + karno + diagtime + age + prior , data = vet)
summary(cox)

cox_fit <- survfit(cox)

plot(cox_fit, main = "cph model", xlab="Days")

autoplot(cox_fit)

# Note that the model flags small cell type, adeno cell type and karno as 
# significant. However, some caution needs to be exercised in interpreting 
# these results. While the Cox Proportional Hazard’s model is thought to be 
# “robust”, a careful analysis would check the assumptions underlying the 
# model. For example, the Cox model assumes that the covariates do not vary 
# with time. In a vignette that accompanies the survival package 
# Therneau, Crowson and Atkinson demonstrate that the Karnofsky score (karno) 
# is, in fact, time-dependent so the assumptions for the Cox model are not 
# met. The vignette authors go on to present a strategy for dealing with time 
# dependent covariates.

# Data scientists who are accustomed to computing ROC curves to assess model 
# performance should be interested in the Concordance statistic. The 
# documentation for the survConcordance() function in the survival package 
# defines concordance as “the probability of agreement for any two randomly 
# chosen observations, where in this case agreement means that the observation 
# with the shorter survival time of the two also has the larger risk score. 
# The predictor (or risk score) will often be the result of a Cox model or 
# other regression” and notes that: “For continuous covariates concordance is 
# equivalent to Kendall’s tau, and for logistic regression is is equivalent to 
# the area under the ROC curve.”

# To demonstrate using the survival package, along with ggplot2 and ggfortify, 
# I’ll fit Aalen’s additive regression model for censored data to the veteran 
# data. The documentation states: “The Aalen model assumes that the cumulative 
# hazard H(t) for a subject can be expressed as a(t) + X B(t), where a(t) is 
# a time-dependent intercept term, X is the vector of covariates for the 
# subject (possibly time-dependent), and B(t) is a time-dependent matrix of 
# coefficients.”

# The plots show how the effects of the covariates change over time. Notice 
# the steep slope and then abrupt change in slope of karno.
aa_fit <-aareg(Surv(time, status) ~ trt + celltype +
                 karno + diagtime + age + prior , 
               data = vet)
aa_fit

#summary(aa_fit)  # provides a more complete summary of results
autoplot(aa_fit)

# Random Forests Model
# As a final example of what some might perceive as a data-science-like way 
# to do time-to-event modeling, I’ll use the ranger() function to fit a 
# Random Forests Ensemble model to the data. Note however, that there is 
# nothing new about building tree models of survival data. Terry Therneau 
# also wrote the rpart package, R’s basic tree-modeling package, along with 
# Brian Ripley. See section 8.4 for the rpart vignette that contains a 
# survival analysis example.

# ranger() builds a model for each observation in the data set. The next 
# block of code builds the model using the same variables used in the Cox 
# model above, and plots twenty random curves, along with a curve that 
# represents the global average for all of the patients. Note that I am 
# using plain old base R graphics here.
# ranger model
r_fit <- ranger(Surv(time, status) ~ trt + celltype + 
                  karno + diagtime + age + prior,
                data = vet,
                mtry = 4,
                importance = "permutation",
                splitrule = "extratrees",
                verbose = TRUE)

# Average the survival models
death_times <- r_fit$unique.death.times 
surv_prob <- data.frame(r_fit$survival)
avg_prob <- sapply(surv_prob,mean)

# Plot the survival models for each patient
plot(r_fit$unique.death.times,r_fit$survival[1,], 
     type = "l", 
     ylim = c(0,1),
     col = "red",
     xlab = "Days",
     ylab = "survival",
     main = "Patient Survival Curves")

#
cols <- colors()
for (n in sample(c(2:dim(vet)[1]), 20)){
  lines(r_fit$unique.death.times, r_fit$survival[n,], type = "l", col = cols[n])
}
lines(death_times, avg_prob, lwd = 2)
legend(500, 0.7, legend = c('Average = black'))

# The next block of code illustrates how ranger() ranks variable importance.
vi <- data.frame(sort(round(r_fit$variable.importance, 4), decreasing = TRUE))
names(vi) <- "importance"
head(vi)

# Notice that ranger() flags karno and celltype as the two most important; 
# the same variables with the smallest p-values in the Cox model. Also note 
# that the importance results just give variable names and not level names. 
# This is because ranger and other tree models do not usually create dummy 
# variables.

# But ranger() does compute Harrell’s c-index, which is similar to the 
# Concordance statistic described 
# above. This is a generalization of the ROC curve, which reduces to 
# the Wilcoxon-Mann-Whitney statistic for binary variables, which in 
# turn, is equivalent to computing the area under the ROC curve.
cat("Prediction Error = 1 - Harrell's c-index = ", r_fit$prediction.error)

# An ROC value of .68 would normally be pretty good for a first try. But 
# note that the ranger model doesn’t do anything to address the time varying 
# coefficients. This apparently is a challenge. In a 2011 paper Hamad observes:

# I believe that the major use for tree-based models for survival data will 
# be to deal with very large data sets.

# Finally, to provide an “eyeball comparison” of the three survival curves, 
# I’ll plot them on the same graph.The following code pulls out the survival 
# data from the three model objects and puts them into a data frame for 
# ggplot().

# Set up for ggplot
kmi <- rep("KM",length(km_fit$time))
km_df <- data.frame(km_fit$time,km_fit$surv,kmi)
names(km_df) <- c("Time","Surv","Model")

coxi <- rep("Cox",length(cox_fit$time))
cox_df <- data.frame(cox_fit$time,cox_fit$surv,coxi)
names(cox_df) <- c("Time","Surv","Model")

rfi <- rep("RF",length(r_fit$unique.death.times))
rf_df <- data.frame(r_fit$unique.death.times,avg_prob,rfi)
names(rf_df) <- c("Time","Surv","Model")

plot_df <- rbind(km_df,cox_df,rf_df)

p <- ggplot(plot_df, aes(x = Time, y = Surv, color = Model))
p + geom_line()

# For this data set, I would put my money on a carefully constructed Cox 
# model that takes into account the time varying coefficients. I suspect 
# that there are neither enough observations nor enough explanatory variables 
# for the ranger() model to do better.

# This four-package excursion only hints at the Survival Analysis tools 
# that are available in R, but it does illustrate some of the richness of 
# the R platform, which has been under continuous development and improvement 
# for nearly twenty years. The ranger package, which suggests the survival 
# package, and ggfortify, which depends on ggplot2 and also suggests the 
# survival package, illustrate how open-source code allows developers to build 
# on the work of their predecessors. The documentation that accompanies the 
# survival package, the numerous online resources, and the statistics such 
# as concordance and Harrell’s c-index packed into the objects produced by 
# fitting the models gives some idea of the statistical depth that underlies 
# almost everything R.

