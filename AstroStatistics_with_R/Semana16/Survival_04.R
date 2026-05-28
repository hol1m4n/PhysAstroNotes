library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condsurv)

# Advanced Topics
# So far we’ve covered:

# -The basics of survival analysis including the Kaplan-Meier survival 
# function and Cox regression
# -Landmark analysis and time-dependent covariates
# -Cumulative incidence and regression for competing risks analyses

# In this section I’ll include a variety of bits and pieces of things that 
# may come up and be handy to know:
  
# 1.- Assessing the proportional hazards assumption
# 2.- Making a smooth survival plot based on x-year survival according to 
# a continuous covariate
# 3.- Conditional survival

# Assessing proportional hazards
# One assumption of the Cox proportional hazards regression model is that 
# the hazards are proportional at each point in time throughout follow-up. 
# The cox.zph() function from the {survival} package allows us to check this 
# assumption. It results in two main things:
  
# 1.- A hypothesis test of whether the effect of each covariate differs 
# according to time, and a global test of all covariates at once.
# -This is done by testing for an interaction effect between the covariate 
# and log(time)
# -A significant p-value indicates that the proportional hazards assumption 
# is violated
# 2.- Plots of the Schoenfeld residuals
# -Deviation from a zero-slope line is evidence that the proportional hazards 
# assumption is violated
mv_fit <- coxph(Surv(time, status) ~ sex + age, data = lung)
cz <- cox.zph(mv_fit)
print(cz)

plot(cz)

# Here we see that with p-values >0.05, we do not reject the null hypothesis, 
# and conclude that the proportional hazards assumption is satisfied for each 
# individual covariate, and also for the model overall.

# Smooth survival plot
# Sometimes you will want to visualize a survival estimate according to a 
# continuous variable. The sm.survival function from the sm package allows 
# you to do this for a quantile of the distribution of survival data. The 
# default quantile is p = 0.5 for median survival.
# install.packages("sm")
library(sm)

sm.options(
  list(
    xlab = "Age (years)",
    ylab = "Median time to death (years)")
)

sm.survival(
  x = lung$age,
  y = lung$time,
  status = lung$status,
  h = sd(lung$age) / nrow(lung)^(-1/4)
)


# -The x’s represent events
# -The o’s represent censoring
# -The line is a smoothed estimate of median survival according to age
#       -In this case, too smooth!
  
# The option h is the smoothing parameter. This should be related to the 
# standard deviation of the continuous covariate, x. Suggested to start with 
# sd(x)n−1/4 then reduce by 1/2, 1/4, etc to get a good amount of smoothing. 
# The previous plot was too smooth so let’s reduce it by 1/6:
sm.survival(
  x = lung$age,
  y = lung$time,
  status = lung$status,
  h = (1/6) * sd(lung$age) / nrow(lung)^(-1/4)
)

# Now we can see that median time to death decreases slightly as age increases.

# Conditional survival
# Sometimes it is of interest to generate survival estimates among a group of 
# patients who have already survived for some length of time.

# S(y|x) = S(x+y)/S(x)

# -y: number of additional survival years of interest
# -x: number of years a patient has already survived

# Zabor, E., Gonen, M., Chapman, P., & Panageas, K. (2013). Dynamic 
# prognostication using conditional survival estimates. Cancer, 119(20), 
# 3589-3592.

# The estimates are easy to generate with basic math on your own, and are 
# also implemented in the {condsurv} package available from 
# https://github.com/zabore/condsurv.

# We can use the conditional_surv_est() function from the {condsurv} package 
# to get estimates and 95% confidence intervals. Let’s condition on survival 
# to 6-months
fit1 <- survfit(Surv(time, status) ~ 1, data = lung)

prob_times <- seq(365.25, 182.625 * 4, 182.625)

# install.packages("kableExtra")
# For dev version
# install.packages("devtools")
# devtools::install_github("haozhu233/kableExtra")
library(kableExtra)

purrr::map_df(
  prob_times, 
  ~conditional_surv_est(
    basekm = fit1, 
    t1 = 182.625, 
    t2 = .x) 
) %>% 
  mutate(months = round(prob_times / 30.4)) %>% 
  select(months, everything()) %>% 
  kable()

# Recall that our initial 1-year survival estimate was 0.41. We see that 
# for patients who have already survived 12 months this increases to 0.58.

# We can also visualize conditional survival data based on different lengths 
# of time survived using the condKMggplot() function from the {condsurv} 
# package:
gg_conditional_surv(
  basekm = fit1, 
  at = prob_times,
  main = "Conditional survival in lung data",
  xlab = "Days"
) +
  labs(color = "Conditional time")

# The resulting plot has one survival curve for each time on which we 
# condition. In this case the first line is the overall survival curve 
# since it is conditioning on time 0.

