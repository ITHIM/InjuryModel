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
  filter(Casualty_Type %in% c("cyclist","pedestrian", "car/taxi"))

ssg <- group_by(ss19, YEAR, Casualty_Type, Age_of_Casualty, Sex_of_Casualty, Casualty_Severity) %>% 
  summarise(count=n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(YEAR, Casualty_Type, Age_of_Casualty, Sex_of_Casualty, Casualty_Severity, fill=list(count=0))

fit <- glm(count ~ Sex_of_Casualty + Age_of_Casualty + Casualty_Type, data=ssg, family=poisson)
summary(fit)

## Including severity as a predictor
fit2 <- glm(count ~ Sex_of_Casualty + Age_of_Casualty + Casualty_Type + Casualty_Severity, data=ssg, family=poisson)
summary(fit2) # lower AIC indicates more efficient model, measured by balance between fit to data vs. number of parameters 

## "Saturated" model including all variables and their full interactions
fitsat <- glm(count ~ Sex_of_Casualty*Age_of_Casualty*Casualty_Type*Casualty_Severity*YEAR, data=ssg, family=poisson)
summary(fitsat) # lower AIC again!

ssg$count_fit <- fitted(fit2)
ssg$count_fitsat <- fitted(fitsat)
plot(ssg$count, ssg$count_fitsat)
abline(a=0,b=1)

## TODO
## Expert consideration of what variables/interactions should always be included
## Some kind of constraints to represent prior knowledge about what should be excluded
## Negative binomial model may fit better
## Put the output of the model into the form we want (predicted count for each combination of categories, and a way of expressing uncertainty about this?)
## ...
