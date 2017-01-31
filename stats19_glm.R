library(tidyverse)
load(file="~/Dropbox/STATS19/combined0514.rda") # contains single object Stats19

## ss19 <- sample_frac(ss19, 0.01) # if we want a 1% random sample

ss19 <- select(Stats19, YEAR, Casualty_Type, Age_of_Casualty, Age_Band_of_Casualty, Sex_of_Casualty, Casualty_Severity) %>%
  mutate(YEAR=factor(YEAR)) %>%    # treat year as categorical for the moment
  filter(Age_of_Casualty != "-1") %>%
  mutate(Age_Band_of_Casualty=factor(Age_Band_of_Casualty)) %>%   ## Age perhaps nicer modelled as continuous: categorical throws away information
  filter(Sex_of_Casualty %in% c("Male","Female")) %>%
  filter(Casualty_Type %in% c("Cyclist","Pedestrian", "Car occupant"))

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
