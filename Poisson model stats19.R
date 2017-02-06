#installing packages
#install.packages("readstata13")
#install.packages("tidyr")
#install.packages("dplyr")

library(readstata13)
library(dplyr)
library(tidyr)

#reading in the data
load("Stats19.rda")
ss19<-tbl_df(ss19)
#adding age bands
ss19$cas_age_band= cut(ss19$cas_age, breaks = c(0, 20,40,60,80,110), right = FALSE)
ss19$strike_age_band= cut(ss19$strike_age, breaks = c(0, 16,25,60,80,110), right = FALSE)
table(ss19$cas_age_band, ss19$strike_age_band)
#poisson model
library(tidyverse)
## ss19 <- sample_frac(ss19, 0.01) # if we want a 1% random sample
###
# [1] "accident_index" "year"           "roadtype"      
# [4] "cas_severity"   "cas_mode"       "cas_male"      
# [7] "strike_mode"    "strike_male"   "strike_age_band"
###
ss19 <- select(ss19, everything()) %>%
  filter(cas_male %in% c(1, 0)) %>%
  filter(cas_mode %in% c("cyclist","pedestrian", "car/taxi"))

ssg <- group_by(ss19, cas_mode, cas_male, cas_severity, strike_mode, cas_age_band, strike_age_band, roadtype, strike_male) %>% 
#  select (ss19, cas_mode, cas_male, cas_severity, strike_mode, cas_age_band, strike_age_band, roadtype, strike_male) %>% 
  summarise(count=n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_mode, cas_male, cas_severity, strike_mode, strike_age_band, cas_age_band, strike_male, roadtype, fill=list(count=0))

##this is the model I want to fit
fit <-
  glm(count ~  strike_mode* roadtype* (cas_mode *cas_severity* cas_age_band* cas_male* strike_age_band  + strike_age_band*strike_male), 
      data=ssg, family=poisson)

##this is the next best model- needs to be done for each strike_mode and road_type
fit <-
  glm(count ~  (cas_mode *cas_severity* cas_age_band* cas_male* strike_age_band  + strike_age_band*strike_male), 
      data=ssg, family=poisson)

#next best model for cars on a roads
ssg<-filter(ssg,roadtype=='A' & strike_mode=='car/taxi')
fit <-
  glm(count ~  (cas_mode *cas_severity* cas_age_band* cas_male* strike_age_band  + strike_age_band*strike_male), 
      data=ssg, family=poisson)
#this is the minimum model needed to run the total model from - if done separately for strike_mode* roadtype* strike_age_band-#
#although strike_age_band is is more complicated
fit <-
  glm(count ~  (cas_mode *cas_severity* cas_age_band* cas_male* strike_age_band), 
      data=ssg, family=poisson)
#this is Excel ithim 1 structure
fit_ITHIM1 <-
  glm(count ~  (cas_mode *cas_severity* roadtype* strike_mode), 
      data=ssg, family=poisson)
## add predicted count with standard error 
pred <- predict(fit, se.fit=TRUE, type="response")
ssg <- mutate(ssg, count_fit=pred[["fit"]], count_se=pred[["se.fit"]])
as.data.frame(ssg)[1:10,]

##I have not gone beyond here -jw
## Predicted counts likely biased and SEs underestimated because we are not including all relevant predictors
## So now fit a "saturated" model including all variables and their full interactions

fitsat <- glm(count ~ cas_male*cas_mode*cas_severity*strike_mode, data=ssg, family=poisson)
pred <- predict(fitsat, se.fit=TRUE, type="response")
ssg <- mutate(ssg, count_fit_sat=pred[["fit"]], count_se_sat=pred[["se.fit"]])

options(scipen=1e+07) # turn off scientific notation for small numbers 
as.data.frame(ssg)[1:10,]
## fitted counts match observed counts more closely, and standard errors are bigger. 


## Model comparison (lower AIC: saturated model is more efficient despite having many more parameters (df))
AIC(fit, fitsat)