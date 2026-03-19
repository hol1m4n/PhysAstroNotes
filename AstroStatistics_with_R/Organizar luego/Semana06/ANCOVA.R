#The Analysis of Covariance (ANCOVA) is used to compare means of 
#an outcome variable between two or more groups taking into account 
#(or to correct for) variability of other variables, called 
#covariates. In other words, ANCOVA allows to compare the adjusted 
#means of two or more independent groups.

#For example, you might want to compare “test score” by “level of 
#education” taking into account the “number of hours spent 
#studying”. In this example: 1) test score is our outcome 
#(dependent) variable; 2) level of education (high school, college 
#degree or graduate degree) is our grouping variable; 3) sudying 
#time is our covariate.

#The one-way ANCOVA can be seen as an extension of the one-way ANOVA 
#that incorporate a covariate variable. The two-way ANCOVA is used 
#to evaluate simultaneously the effect of two independent grouping 
#variables (A and B) on an outcome variable, after adjusting for one 
#or more continuous variables, called covariates.

#In this article, you will learn how to:
#Compute and interpret the one-way and the two-way ANCOVA in R
#Check ANCOVA assumptions
#Perform post-hoc tests, multiple pairwise comparisons between 
#       groups to identify which groups are different
#Visualize the data using box plots, add ANCOVA and pairwise 
#       comparisons p-values to the plot

#######################################################################
#Assumptions

#ANCOVA makes several assumptions about the data, such as:
#Linearity between the covariate and the outcome variable 
#     at each level of the grouping variable. This can be 
#     checked by creating a grouped scatter plot of the 
#     covariate and the outcome variable.
#Homogeneity of regression slopes. The slopes of the regression 
#     lines, formed by the covariate and the outcome variable, 
#     should be the same for each group. This assumption evaluates 
#     that there is no interaction between the outcome and the 
#     covariate. The plotted regression lines by groups should 
#     be parallel.
#The outcome variable should be approximately normally distributed. 
#     This can be checked using the Shapiro-Wilk test of normality 
#     on the model residuals.
#Homoscedasticity or homogeneity of residuals variance for all 
#     groups. The residuals are assumed to have a constant variance 
#     (homoscedasticity)
#No significant outliers in the groups

####################################################################
#Prerequisites

#Make sure you have installed the following R packages:
  
#tidyverse for data manipulation and visualization
#ggpubr for creating easily publication ready plots
#rstatix for easy pipe-friendly statistical analyses
#broom for printing a nice summary of statistical tests as data frames
#datarium: contains required data sets for this chapter

#Start by loading the following required packages:
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)
library(datarium)

########One-way ANCOVA###############
#Data preparation

#We’ll prepare our demo data from the anxiety dataset available 
#in the datarium package.
#Researchers investigated the effect of exercises in reducing the 
#level of anxiety. Therefore, they conducted an experiment, where 
#they measured the anxiety score of three groups of individuals 
#practicing physical exercises at different levels (grp1: low, 
#grp2: moderate and grp3: high).
#The anxiety score was measured pre- and 6-months post-exercise 
#training programs. It is expected that any reduction in the anxiety 
#by the exercises programs would also depend on the participant’s 
#basal level of anxiety score.
#In this analysis we use the pretest anxiety score as the covariate 
#and are interested in possible differences between group with respect 
#to the post-test anxiety scores.

# Load and prepare the data
data("anxiety", package = "datarium")
anxiety <- anxiety %>%
  select(id, group, t1, t3) %>%
  rename(pretest = t1, posttest = t3)
anxiety[14, "posttest"] <- 19

# Inspect the data by showing one random row by groups
set.seed(123)
anxiety %>% sample_n_by(group, size = 1)

#Check assumptions
#Linearity assumption
#Create a scatter plot between the covariate (i.e., pretest) and 
#   the outcome variable (i.e., posttest)
#Add regression lines, show the corresponding equations and the 
#   R2 by groups

ggscatter(
  anxiety, x = "pretest", y = "posttest",
  color = "group", add = "reg.line"
)+
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group)
  )

#Homogeneity of regression slopes
#This assumption checks that there is no significant interaction 
#     between the covariate and the grouping variable. This can 
#     be evaluated as follow:
anxiety %>% anova_test(posttest ~ group*pretest)

#Normality of residuals
#You first need to compute the model using lm(). In R, you can 
#easily augment your data to add fitted values and residuals by 
#using the function augment(model) [broom package]. Let’s call 
#the output model.metrics because it contains several metrics 
#useful for regression diagnostics.

# Fit the model, the covariate goes first
model <- lm(posttest ~ pretest + group, data = anxiety)

# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model.metrics, 3)

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)

#Homogeneity of variances
#ANCOVA assumes that the variance of the residuals is equal for 
#all groups. This can be checked using the Levene’s test:
model.metrics %>% levene_test(.resid ~ group)

#Outliers
#An outlier is a point that has an extreme outcome variable value. 
#The presence of outliers may affect the interpretation of the model.

#Outliers can be identified by examining the standardized residual 
#(or studentized residual), which is the residual divided by its 
#estimated standard error. Standardized residuals can be interpreted 
#as the number of standard errors away from the regression line.
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

#Computation
#The orders of variables matters when computing ANCOVA. You want to 
#remove the effect of the covariate first - that is, you want to 
#control for it - prior to entering your main variable or interest.
res.aov <- anxiety %>% anova_test(posttest ~ pretest + group)
get_anova_table(res.aov)


#Post-hoc test
#Pairwise comparisons can be performed to identify which groups are 
#different. The Bonferroni multiple testing correction is applied. 
#This can be easily done using the function emmeans_test() 
#[rstatix package], a wrapper around the emmeans package, which needs 
#to be installed. Emmeans stands for estimated marginal means (aka 
#least square means or adjusted means).

# Pairwise comparisons
library(emmeans)
pwc <- anxiety %>% 
  emmeans_test(
    posttest ~ group, covariate = pretest,
    p.adjust.method = "bonferroni"
  )
pwc

# Display the adjusted means of each group
# Also called as the estimated marginal means (emmeans)
get_emmeans(pwc)

#Report
#An ANCOVA was run to determine the effect of exercises on the 
#anxiety score after controlling for basal anxiety score of participants.

#After adjustment for pre-test anxiety score, there was a statistically 
#significant difference in post-test anxiety score between the groups, 
#F(2, 41) = 218.63, p < 0.0001.

#Post hoc analysis was performed with a Bonferroni adjustment. The mean 
#anxiety score was statistically significantly greater in grp1 
#(16.4 +/- 0.15) compared to the grp2 (15.8 +/- 0.12) and grp3 
#(13.5 +/_ 0.11), p < 0.001.

# Visualization: line plots with p-values
pwc <- pwc %>% add_xy_position(x = "group", fun = "mean_se")
ggline(get_emmeans(pwc), x = "group", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) + 
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

##############Two-way ANCOVA#################
#Data preparation
#We’ll use the stress dataset available in the datarium package. 
#In this study, a researcher wants to evaluate the effect of treatment 
#and exercise on stress reduction score after adjusting for age.

#In this example: 1) stress score is our outcome (dependent) 
#variable; 2) treatment (levels: no and yes) and exercise 
#(levels: low, moderate and high intensity training) are our 
#grouping variable; 3) age is our covariate.

#Load the data and show some random rows by groups:
data("stress", package = "datarium")
stress %>% sample_n_by(treatment, exercise)

#Check assumptions
#Linearity assumption
#Create a scatter plot between the covariate (i.e., age) and the 
#    outcome variable (i.e., score) for each combination of the 
#    groups of the two grouping variables:
#Add smoothed loess lines, which helps to decide if the relationship 
#is linear or not
ggscatter(
  stress, x = "age", y = "score",
  facet.by  = c("exercise", "treatment"), 
  short.panel.labs = FALSE
)+
  stat_smooth(method = "loess", span = 0.9)

#Homogeneity of regression slopes
#This assumption checks that there is no significant interaction 
#between the covariate and the grouping variables. This can be 
#evaluated as follow:
stress %>%
  anova_test(
    score ~ age + treatment + exercise + 
      treatment*exercise + age*treatment +
      age*exercise + age*exercise*treatment
  )

#Another simple alternative is to create a new grouping variable, 
#say group, based on the combinations of the existing variables, 
#and then compute ANOVA model:
stress %>%
  unite(col = "group", treatment, exercise) %>%
  anova_test(score ~ group*age)

#Normality of residuals
# Fit the model, the covariate goes first
model <- lm(score ~ age + treatment*exercise, data = stress)

# Inspect the model diagnostic metrics
model.metrics <- augment(model) %>%
  select(-.hat, -.sigma, -.fitted) # Remove details
head(model.metrics, 3)

# Assess normality of residuals using shapiro wilk test
shapiro_test(model.metrics$.resid)

#Homogeneity of variances
#ANCOVA assumes that the variance of the residuals is equal for 
#all groups. This can be checked using the Levene’s test:
levene_test(.resid ~ treatment*exercise, data = model.metrics)

#Outliers
#Observations whose standardized residuals are greater than 3 
#in absolute value are possible outliers.
model.metrics %>% 
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

#Computation
res.aov <- stress %>% 
  anova_test(score ~ age + treatment*exercise)
get_anova_table(res.aov)

#Post-hoc test
#A statistically significant two-way interactions can be followed 
#up by simple main effect analyses, that is evaluating the effect 
#of one variable at each level of the second variable, and vice-versa.

#In the situation, where the interaction is not significant, you can 
#report the main effect of each grouping variable.

#A significant two-way interaction indicates that the impact that 
#one factor has on the outcome variable depends on the level of the 
#other factor (and vice versa). So, you can decompose a significant 
#two-way interaction into:
  
#Simple main effect: run one-way model of the first variable (factor A) 
#     at each level of the second variable (factor B),
#Simple pairwise comparisons: if the simple main effect is significant, 
#     run multiple pairwise comparisons to determine which groups are 
#     different.

#For a non-significant two-way interaction, you need to determine
#whether you have any statistically significant main effects from 
#the ANCOVA output.

#In this section we’ll describe the procedure for a significant 
#three-way interaction

###################################################################
#Simple main effect analyses for treatment
#Analyze the simple main effect of treatment at each level of 
#exercise. Group the data by exercise and perform one-way ANCOVA 
#for treatment controlling for age:

# Effect of treatment at each level of exercise
stress %>%
  group_by(exercise) %>%
  anova_test(score ~ age + treatment)

#Compute pairwise comparisons between treatment groups at each 
#level of exercise. The Bonferroni multiple testing correction 
#is applied.

# Pairwise comparisons
pwc <- stress %>% 
  group_by(exercise) %>%
  emmeans_test(
    score ~ treatment, covariate = age,
    p.adjust.method = "bonferroni"
  )
pwc %>% filter(exercise == "high")

#Simple main effect for exercise
#You can do the same post-hoc analyses for the exercise variable at 
#each level of treatment variable.

# Effect of exercise at each level of treatment
stress %>%
  group_by(treatment) %>%
  anova_test(score ~ age + exercise)

#Perform multiple pairwise comparisons between exercise groups at each 
#level of treatment. You don’t need to interpret the results for the 
#“no treatment” group, because the effect of exercise was not significant 
#for this group.
pwc2 <- stress %>% 
  group_by(treatment) %>%
  emmeans_test(
    score ~ exercise, covariate = age,
    p.adjust.method = "bonferroni"
  ) %>%
  select(-df, -statistic, -p) # Remove details
pwc2 %>% filter(treatment == "yes")

#Report
#A two-way ANCOVA was performed to examine the effects of treatment 
#and exercise on stress reduction, after controlling for age.

#There was a statistically significant two-way interaction between 
#treatment and exercise on score concentration, whilst controlling 
#for age, F(2, 53) = 4.45, p = 0.016.

#Therefore, an analysis of simple main effects for exercise and 
#treatment was performed with statistical significance receiving a 
#Bonferroni adjustment and being accepted at the p < 0.025 level 
#for exercise and p < 0.0167 for treatment.

#The simple main effect of treatment was statistically significant 
#in the high-intensity exercise group (p = 0.00046), but not in the 
#low-intensity exercise group (p = 0.52) and the moderate-intensity 
#exercise group (p = 0.53).

#The effect of exercise was statistically significant in the 
#treatment=yes group (p < 0.0001), but not in the treatment=no 
#group (p = 0.031).

#All pairwise comparisons were computed for statistically significant 
#simple main effects with reported p-values Bonferroni adjusted. For 
#the treatment=yes group, there was a statistically significant 
#difference between the adjusted mean of low and high exercise group 
#(p < 0.0001) and, between moderate and high group (p < 0.0001). 
#The difference between the adjusted means of low and moderate exercise 
#groups was not significant.

#Create a line plot:
# Line plot
lp <- ggline(
  get_emmeans(pwc), x = "exercise", y = "emmean", 
  color = "treatment", palette = "jco"
) +
  geom_errorbar(
    aes(ymin = conf.low, ymax = conf.high, color = treatment), 
    width = 0.1
  )

#Add p-values
# Comparisons between treatment group at each exercise level
pwc <- pwc %>% add_xy_position(x = "exercise", fun = "mean_se", step.increase = 0.2)
pwc.filtered <- pwc %>% filter(exercise == "high")
lp + 
  stat_pvalue_manual(
    pwc.filtered, hide.ns = TRUE, tip.length = 0,
    bracket.size = 0
  ) +
  labs(
    subtitle = get_test_label(res.aov,  detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# Comparisons between exercises group at each treatment level
pwc2 <- pwc2 %>% add_xy_position(x = "exercise", fun = "mean_se")
pwc2.filtered <- pwc2 %>% filter(treatment == "yes")
lp + 
  stat_pvalue_manual(
    pwc2.filtered, hide.ns = TRUE, tip.length = 0,
    step.group.by = "treatment", color = "treatment"
  ) +
  labs(
    subtitle = get_test_label(res.aov,  detailed = TRUE),
    caption = get_pwc_label(pwc2)
  )

#Summary
#This article describes how to compute and interpret one-way and 
#two-way ANCOVA in R. We also explain the assumptions made by ANCOVA 
#tests and provide practical examples of R codes to check whether 
#the test assumptions are met or not.

