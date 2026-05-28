library(magrittr)
library(dplyr)
library(survival)
library(lubridate)
library(ggsurvfit)
library(gtsummary)
library(tidycmprsk)
library(condsurv)

# Overall survival is measured from treatment start, and interest is in the 
# association between complete response to treatment and survival.

# Anderson et al (JCO, 1983) described why traditional methods such as 
# log-rank tests or Cox regression are biased in favor of responders in 
# this scenario, and proposed the landmark approach. The null hypothesis 
# in the landmark approach is that survival from landmark does not depend 
# on response status at landmark.

# Anderson, J., Cain, K., & Gelber, R. (1983). Analysis of survival by 
# tumor response. Journal of Clinical Oncology : Official Journal of the 
# American Society of Clinical Oncology, 1(11), 710-9.

# Some other possible covariates of interest in cancer research that may 
# not be measured at baseline include:
  
# -tranplant failure
# -graft versus host disease
# -second resection
# -adjuvant therapy
# -compliance
# -adverse events

# The BMT dataset
# Throughout this section we will use the BMT dataset from {SemiCompRisks} 
# package as an example dataset. The data consist of 137 bone marrow transplant 
# patients. Variables of interest include:
  
# -T1 time (in days) to death or last follow-up
# -delta1 death indicator; 1=Dead, 0=Alive
# -TA time (in days) to acute graft-versus-host disease
# -deltaA acute graft-versus-host disease indicator; 1-Developed acute 
# graft-versus-host disease, 0-Never developed acute graft-versus-host disease

#First, load the data for use in examples throughout:
# install.packages("SemiCompRisks")
data(BMT, package = "SemiCompRisks")

#Here are the first 6 observations:
head(BMT[, c("T1", "delta1", "TA", "deltaA")])

# Landmark approach
# 1.- Select a fixed time after baseline as your landmark time. Note: this 
# should be done based on clinical information, prior to data inspection
# 2.- Subset population for those followed at least until landmark time. 
# Note: always report the number excluded due to the event of interest or 
# censoring prior to the landmark time.
# 3.- Calculate follow-up from landmark time and apply traditional log-rank 
# tests or Cox regression.

# In the BMT data, interest is in the association between acute graft versus 
# host disease (aGVHD) and survival. But aGVHD is assessed after the transplant, 
# which is our baseline, or start of follow-up, time.

# Step 1 Select landmark time

# Typically aGVHD occurs within the first 90 days following transplant, so we 
# use a 90-day landmark.

# Step 2 Subset population for those followed at least until landmark time
lm_dat <- 
  BMT %>% 
  filter(T1 >= 90) 

# We exclude 15 patients who were not followed until the landmark time of 
# 90 days.

# Note: All 15 excluded patients died before the 90 day landmark.

# Step 3 Calculate follow-up time from landmark and apply traditional methods.
lm_dat <- 
  lm_dat %>% 
  mutate(
    lm_T1 = T1 - 90
  )

survfit2(Surv(lm_T1, delta1) ~ deltaA, data = lm_dat) %>% 
  ggsurvfit() +
  labs(
    x = "Days from 90-day landmark",
    y = "Overall survival probability"
  ) +
  add_risktable()

# In Cox regression you can use the subset option in coxph to exclude those 
# patients who were not followed through the landmark time, and we can view 
# the results using the tbl_regression() function from the {gtsummary} package:
coxph(
  Surv(T1, delta1) ~ deltaA, 
  subset = T1 >= 90, 
  data = BMT
)

# Time-dependent covariate approach
# An alternative to a landmark analysis is incorporation of a time-dependent 
# covariate. This may be more appropriate than landmark analysis when:

# -the value of a covariate is changing over time
# -there is not an obvious landmark time
# -use of a landmark would lead to many exclusions

# Analysis of time-dependent covariates requires setup of a special dataset, 
# in a format known as counting process format. See the detailed paper on this 
# by the author of the {survival} package Using Time Dependent Covariates and 
# Time Dependent Coefficients in the Cox Model.

# There was no ID variable in the BMT data, which is needed to create the 
# special dataset, so create an ID variable called my_id:
library(tibble)
BMT <- rowid_to_column(BMT, "my_id")

# Use the tmerge function with the event and tdc function options to create 
# the special dataset.

# -tmerge() creates a long dataset with multiple time intervals for the 
# different covariate values for each patient
# -event() creates the new event indicator to go with the newly created 
# time intervals
# -tdc() creates the time-dependent covariate indicator to go with the 
# newly created time intervals
td_dat <- 
  tmerge(
    data1 = BMT %>% select(my_id, T1, delta1), 
    data2 = BMT %>% select(my_id, T1, delta1, TA, deltaA), 
    id = my_id, 
    death = event(T1, delta1),
    agvhd = tdc(TA)
  )

# We see that patients 1 and 4 now have multiple rows of data because they 
# both developed aGVHD at some point in time after baseline.

# Now we can analyze this time-dependent covariate as usual using Cox 
# regression with coxph and an alteration to our use of Surv to include 
# arguments to both time and time2:
coxph(
  Surv(time = tstart, time2 = tstop, event = death) ~ agvhd, 
  data = td_dat
)

# We find that acute graft versus host disease is not significantly associated 
# with death using either landmark analysis or a time-dependent covariate.

# Often one will want to use landmark analysis for visualization of a single 
# covariate, and Cox regression with a time-dependent covariate for univariable 
# and multivariable modeling.

