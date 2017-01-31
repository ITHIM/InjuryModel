library(tidyverse)
load(file="Stats19_05-15_ready_31_1_2017.rda") # contains single object Stats19
## ss19 <- sample_frac(ss19, 0.01) # if we want a 1% random sample

###
# [1] "accident_index" "year"           "roadtype"      
# [4] "cas_severity"   "cas_mode"       "cas_male"      
# [7] "strike_mode"    "strike_male"   
###

ss19 <- select(d, everything()) %>%
  mutate(year=factor(year)) %>%    # treat year as categorical for the moment
  filter(cas_male %in% c(1, 0)) %>%
  filter(cas_mode %in% c("cyclist","pedestrian", "car/taxi"))

ssg <- group_by(ss19, year, cas_mode, cas_male, cas_severity, strike_mode) %>% 
  summarise(count=n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(year, cas_mode, cas_male, cas_severity, strike_mode, fill=list(count=0)) %>%
  mutate(cas_dist = ifelse(cas_mode=="pedestrian", 134.6,
                           ifelse(cas_mode=="cyclist", 30.9,
                                  ifelse(cas_mode=="car/taxi", 4000.1, NA))))

fit <- glm(count ~ cas_male + cas_mode + cas_severity + strike_mode, data=ssg, family=poisson)
## note: Can't include both casualty mode and distance in the same model if distance is just a function of mode

## add predicted count with standard error 
pred <- predict(fit, se.fit=TRUE, type="response")
ssg <- mutate(ssg, count_fit=pred[["fit"]], count_se=pred$se.fit)
as.data.frame(ssg)[1:10,]

## Predicted counts likely biased and SEs underestimated because we are not including all relevant predictors
## So now fit a "saturated" model including all variables and their full interactions

fitsat <- glm(count ~ cas_male*cas_mode*cas_severity*strike_mode, data=ssg, family=poisson)
pred <- predict(fitsat, se.fit=TRUE, type="response")
ssg <- mutate(ssg, count_fit_sat=pred[["fit"]], count_se_sat=pred$se.fit)

options(scipen=1e+07) # turn off scientific notation for small numbers 
as.data.frame(ssg)[1:10,]
## fitted counts match observed counts more closely, and standard errors are bigger. 


## Model comparison (lower AIC: saturated model is more efficient despite having many more parameters (df))
AIC(fit, fitsat)


## TODO
## Include distance properly 
## Expert consideration of what variables/interactions should always be included
## Some kind of constraints to represent prior knowledge about what should be excluded
## Negative binomial model may fit better
## ...
