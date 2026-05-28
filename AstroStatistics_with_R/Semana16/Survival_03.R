library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condsurv)

# Competing Risks
# Competing risks analyses may be used when subjects have multiple possible 
# events in a time-to-event setting.

# Examples: (1) recurrence, (2) death from disease, (3)death from other 
# causes and (4) treatment response

# All or some of these (among others) may be possible events in any given study.

# The fundamental problem that may lead to the need for specialized statistical 
# methods is unobserved dependence among the various event times. For example, 
# one can imagine that patients who recur are more likely to die, and therefore 
# times to recurrence and times to death would not be independent events.

# There are two approaches to analysis in the presence of multiple potential 
# outcomes:
  
# 1.- Cause-specific hazard of a given event: this represents the rate per 
# unit of time of the event among those not having failed from other events
# 2.- Subdistribution hazard of a given event: this represents the rate per 
# unit of time of the event as well as the influence of competing events

# Each of these approaches may only illuminate one important aspect of the 
# data while possibly obscuring others, and the chosen approach should depend 
# on the question of interest.

# When the events are independent (almost never true), cause-specific hazards 
# is unbiased. When the events are dependent, a variety of results can be 
# obtained depending on the setting.

# Cumulative incidence using 1 minus the Kaplan-Meier estimate is always >= 
# cumulative incidence using competing risks methods, so can only lead to an 
# overestimate of the cumulative incidence, though the amount of overestimation 
# depends on event rates and dependence among events.

# To establish that a covariate is indeed acting on the event of interest, 
# cause-specific hazards may be preferred for treatment or prognostic marker 
# effect testing. To establish overall benefit, subdistribution hazards may 
# be preferred for building prognostic nomograms or considering health economic 
# effects to get a better sense of the influence of treatment and other 
# covariates on an absolute scale.

# We can obtain the cause-specific hazard of a given event using the methods 
# introduced in the previous section, where the event of interest counts as 
# an event and any competing events are censored at the date of the competing 
# even.

# The rest of this section will focus on methods for subdistribtuion hazards.

# The primary package we will use for competing risks analysis is the 
# {tidycmprsk} package.

# The Melanoma dataset
# We will use the Melanoma data from the {MASS} package to illustrate these 
# concepts. It contains variables:
  
# -time survival time in days, possibly censored.
# -status 1 died from melanoma, 2 alive, 3 dead from other causes.
# -sex 1 = male, 0 = female
# -age age in years.
# -year of operation.
# -thickness tumor thickness in mm.
# -ulcer 1 = presence, 0 = absence.

# install.packages("MASS")
data(Melanoma, package = "MASS")

# The status variable in these data are coded in a non-standard way. 
# Let’s recode to avoid confusion:
Melanoma <- 
  Melanoma %>% 
  mutate(
    status = as.factor(recode(status, `2` = 0, `1` = 1, `3` = 2))
  )

# Now we have:
# -status 0=alive, 1=died from melanoma, 2=dead from other causes.

# Here are the first 6 patients:
head(Melanoma)

# Cumulative incidence for competing risks
# A non-parametric estimate of the cumulative incidence of the event of 
# interest. At any point in time, the sum of the cumulative incidence of 
# each event is equal to the total cumulative incidence of any event (not 
# true in the cause-specific setting). Gray’s test is a modified Chi-squared 
# test used to compare 2 or more groups.

# Estimate the cumulative incidence in the context of competing risks using 
# the cuminc function from the {tidycmprsk} package. By default this requires 
# the status to be a factor variable with censored patients coded as 0.
cuminc(Surv(time, status) ~ 1, data = Melanoma)

# We can use the ggcuminc() function from the {ggsurvfit} package to plot the 
# cumulative incidence. By default it plots the first event type only. So the 
# following plot shows the cumulative incidence of death from melanoma:
cuminc(Surv(time, status) ~ 1, data = Melanoma) %>% 
  ggcuminc() + 
  labs(
    x = "Days"
  ) + 
  add_confidence_interval() +
  add_risktable()

# If we want to include both event types, specify the outcomes in the 
# ggcuminc(outcome=) argument:
cuminc(Surv(time, status) ~ 1, data = Melanoma) %>% 
  ggcuminc(outcome = c("1", "2")) +
  ylim(c(0, 1)) + 
  labs(
    x = "Days"
  )

# Now let’s say we wanted to examine death from melanoma or other causes in 
# the Melanoma data, according to ulcer, the presence or absence of ulceration. 
cuminc(Surv(time, status) ~ ulcer, data = Melanoma) 

# Then we can see the plot of death due to melanoma, according to ulceration 
# status, as before using ggcuminc() from the {ggsurvfit} package:
cuminc(Surv(time, status) ~ ulcer, data = Melanoma) %>% 
  ggcuminc() + 
  labs(
    x = "Days"
  ) + 
  add_confidence_interval() +
  add_risktable()

# Competing risks regression
# There are two approaches to competing risks regression:
  
# 1.- Cause-specific hazards
# -instantaneous rate of occurrence of the given type of event in subjects 
# who are currently event‐free
# -estimated using Cox regression (coxph function)
# 2.- Subdistribution hazards
# -instantaneous rate of occurrence of the given type of event in subjects 
# who have not yet experienced an event of that type
# -estimated using Fine-Gray regression (crr function)

# Let’s say we’re interested in looking at the effect of age and sex on death 
# from melanoma, with death from other causes as a competing event.

# The crr() function from the {tidycmprsk} package will estimate the 
# subdistribution hazards.
crr(Surv(time, status) ~ sex + age, data = Melanoma)

# We see that male sex (recall that 1=male, 0=female in these data) is 
# significantly associated with increased hazard of death due to melanoma, 
# whereas age was not significantly associated with death due to melanoma.

# Alternatively, if we wanted to use the cause-specific hazards regression 
# approach, we first need to censor all subjects who didn’t have the event 
# of interest, in this case death from melanoma, and then use coxph as before. 
# So patients who died from other causes are now censored for the 
# cause-specific hazard approach to competing risks.
coxph(
  Surv(time, ifelse(status == 1, 1, 0)) ~ sex + age, 
  data = Melanoma
)

# And in this case we obtain similar results using the two approaches to 
# competing risks regression.

