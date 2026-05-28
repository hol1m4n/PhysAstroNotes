# install.packages(c("survival", "lubridate", "ggsurvfit", "gtsummary", "tidycmprsk"))
# remotes::install_github("zabore/condsurv")
# remotes::install_github("zabore/ezfun")
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condsurv)

# The lung dataset
# Throughout this section, we will use the lung dataset from the {survival} 
# package as example data. The data contain subjects with advanced lung cancer 
# from the North Central Cancer Treatment Group. We will focus on the following 
# variables throughout this tutorial:
lung = read.csv("/home/holman/Downloads/Semana16/lung_cancer.csv", header=TRUE)  

# time: Observed survival time in days
# status: censoring status 1=censored, 2=dead
# sex: 1=Male, 2=Female

# Note that the status is coded in a non-standard way in this dataset. 
# Typically you will see 1=event, 0=censored. Let’s recode it to avoid 
# confusion:
library(dplyr)

lung <- 
  lung %>% 
  mutate(
    status = recode(status, '1' = 0, '2' = 1)
  )

# Now we have:
# time: Observed survival time in days
# status: censoring status 0=censored, 1=dead
# sex: 1=Male, 2=Female

# Here are the first 6 observations:
head(lung[, c("time", "status", "sex")])

# Note: the Surv() function in the {survival} package accepts by default 
# TRUE/FALSE, where TRUE is event and FALSE is censored; 1/0 where 1 is event 
# and 0 is censored; or 2/1 where 2 is event and 1 is censored. Please take 
# care to ensure the event indicator is properly formatted.

# Calculating survival times
# Data will often come with start and end dates rather than pre-calculated 
# survival times. The first step is to make sure these are formatted as dates 
# in R.

# Let’s create a small example dataset with variables sx_date for surgery 
# date and last_fup_date for the last follow-up date:
date_ex <- 
  tibble(
    sx_date = c("2007-06-22", "2004-02-13", "2010-10-27"), 
    last_fup_date = c("2017-04-15", "2018-07-04", "2016-10-31")
  )

date_ex

# We see these are both character variables, but we need them to be formatted 
# as dates.

# We will use the {lubridate} package to work with dates. In this case, we 
# need to use the ymd() function to change the format, since the dates are 
# currently in the character format where the year comes first, followed by 
# the month, and followed by the day.
date_ex <-
  date_ex %>% 
  mutate(
    sx_date = ymd(sx_date), 
    last_fup_date = ymd(last_fup_date)
  )

date_ex

# Now we see that the two dates are formatted as date rather than as 
# character. Access the help page with ?ymd to see all date format options.

# Now that the dates are formatted, we need to calculate the difference 
# between start and end dates in some units, usually months or years. Using 
# the {lubridate} package, the operator %--% designates a time interval, 
# which is then converted to the number of elapsed seconds using as.duration() 
# and finally converted to years by dividing by dyears(1), which gives the 
# number of seconds in a year.
date_ex <-
  date_ex %>% 
  mutate(
    os_yrs = as.duration(sx_date %--% last_fup_date) / dyears(1)
  )

date_ex

# Now we have our observed time for use in survival analysis.

# Note: we need to load the {lubridate} package using a call to library in 
# order to be able to access the special operators (similar to situation 
# with pipes - i.e. we can’t use lubridate::ymd() and then expect to use the 
# special operators).

# Creating survival objects and curves
# The Kaplan-Meier method is the most common way to estimate survival times 
# and probabilities. It is a non-parametric approach that results in a step 
# function, where there is a step down each time an event occurs.

# The Surv() function from the {survival} package creates a survival object 
# for use as the response in a model formula. There will be one entry for each 
# subject that is the survival time, which is followed by a + if the subject 
# was censored. Let’s look at the first 10 observations:
Surv(lung$time, lung$status)[1:10]

# We see that subject 1 had an event at time 306 days, subject 2 had an event 
# at time 455 days, subject 3 was censored at time 1010 days, etc.

# The survfit() function creates survival curves using the Kaplan-Meier method 
# based on a formula. Let’s generate the overall survival curve for the entire 
# cohort, assign it to object s1, and look at the structure using str():
s1 <- survfit(Surv(time, status) ~ 1, data = lung)
str(s1)

# Some key components of this survfit object that will be used to create 
# survival curves include:
# time: the timepoints at which the curve has a step, i.e. at least one 
# event occurred
# surv: the estimate of survival at the corresponding time

# Kaplan-Meier plots
# We will use the {ggsurvfit} package to generate Kaplan-Meier plots. This 
# package aims to ease plotting of time-to-event endpoints using the power 
# of the {ggplot2} package.

# Note: alternatively, survival plots can be created using base R or the 
# {survminer} package.

# The {ggsurvfit} package works best if you create the survfit object using 
# the included ggsurvfit::survfit2() function, which uses the same syntax to 
# what we saw previously with survival::survfit(). The ggsurvfit::survfit2() 
# tracks the environment from the function call, which allows the plot to 
# have better default values for labeling and p-value reporting.
survfit2(Surv(time, status) ~ 1, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  )

# The default plot in ggsurvfit() shows the step function only. We can add 
# the confidence interval using add_confidence_interval():
survfit2(Surv(time, status) ~ 1, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()

# Typically we will also want to see the numbers at risk in a table below 
# the x-axis. We can add this using add_risktable():
survfit2(Surv(time, status) ~ 1, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval() +
  add_risktable()

# Plots can be customized using many standard {ggplot2} options.

# Estimating x-year survival
# One quantity often of interest in a survival analysis is the probability of 
# surviving beyond a certain number of years, x.

# For example, to estimate the probability of surviving to 1 year, use summary 
# with the times argument (Note: the time variable in the lung data is actually 
# in days, so we need to use times = 365.25)
summary(survfit(Surv(time, status) ~ 1, data = lung), times = 365.25)

# We find that the 1-year probability of survival in this study is 41%.

# The associated lower and upper bounds of the 95% confidence interval are also 
# displayed.

# The 1-year survival probability is the point on the y-axis that corresponds 
# to 1 year on the x-axis for the survival curve.

# What happens if you use a “naive” estimate?
# 121 of the 228 patients in the lung data died by 1 year so the “naive” 
# estimate is calculated as:
#  (1−121/228)×100=47%

# You get an incorrect estimate of the 1-year probability of survival when 
# you ignore the fact that 42 patients were censored before 1 year.

# Recall the correct estimate of the 1-year probability of survival, accounting 
# for censoring using the Kaplan-Meier method, was 41%.

# Ignoring censoring leads to an overestimate of the overall survival 
# probability. Imagine two studies, each with 228 subjects. There are 165 
# deaths in each study. Censoring is ignored in one (pink line), censoring 
# is accounted for in the other (blue line). The censored subjects only 
# contribute information for a portion of the follow-up time, and then fall 
# out of the risk set, thus pulling down the cumulative probability of 
# survival. Ignoring censoring erroneously treats patients who are censored 
# as part of the risk set for the entire follow-up period.

# Estimating median survival time
# Another quantity often of interest in a survival analysis is the average 
# survival time, which we quantify using the median. Survival times are not 
# expected to be normally distributed so the mean is not an appropriate summary.

# We can obtain the median survival directly from the survfit object:
survfit(Surv(time, status) ~ 1, data = lung)

# We see the median survival time is 310 days The lower and upper bounds of 
# the 95% confidence interval are also displayed.

# Median survival is the time corresponding to a survival probability of 0.5:

# What happens if you use a “naive” estimate?
  
# Summarize the median survival time among the 165 patients who died:
lung %>% 
  filter(status == 1) %>% 
  summarize(median_surv = median(time))

# You get an incorrect estimate of median survival time of 226 days when you 
# ignore the fact that censored patients also contribute follow-up time.

# Recall the correct estimate of median survival time is 310 days.

# Ignoring censoring will lead to an underestimate of median survival time 
# because the follow-up time that censored patients contribute is excluded 
# (pink line). The true survival curve accounting for censoring in the lung 
# data is shown in blue for comparison.

# Comparing survival times between groups

# We can conduct between-group significance tests using a log-rank test. The 
# log-rank test equally weights observations over the entire follow-up time 
# and is the most common way to compare survival times between groups. There 
# are versions that more heavily weight the early or late follow-up that could 
# be more appropriate depending on the research question (see ?survdiff for 
# different test options).
survfit2(Surv(time, status) ~ sex, data = lung) %>% 
  ggsurvfit() +
  labs(
    x = "Days",
    y = "Overall survival probability"
  ) + 
  add_confidence_interval()

# We get the log-rank p-value using the survdiff function. For example, we can 
# test whether there was a difference in survival time according to sex in the 
# lung data:
survdiff(Surv(time, status) ~ sex, data = lung)

# We see that there was a significant difference in overall survival according 
# to sex in the lung data, with a p-value of p = 0.001.

# The Cox regression model
# We may want to quantify an effect size for a single variable, or include more 
# than one variable into a regression model to account for the effects of 
# multiple variables.

# The Cox regression model is a semi-parametric model that can be used to fit 
# univariable and multivariable regression models that have survival outcomes.
# h(t|Xi)=h0(t)exp(β1Xi1+⋯+βpXip)

# h(t): hazard, or the instantaneous rate at which events occur 
# h0(t): underlying baseline hazard

# Some key assumptions of the model: (1) non-informative censoring and 
# (2) proportional hazards

# Note: parametric regression models for survival outcomes are also available, 
# but they won’t be addressed in this tutorial

# We can fit regression models for survival data using the coxph() function 
# from the {survival} package, which takes a Surv() object on the left hand 
# side and has standard syntax for regression formulas in R on the right hand 
# side.
coxph(Surv(time, status) ~ sex, data = lung)

