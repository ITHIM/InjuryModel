## Demo of how to do alternative scenarios in Poisson model 

## The idea is to implement the log-linear model currently used in the Analytica model for safety in numbers

## rate(i) = base_rate(i) * av(i)^bv * cv(i) * as(i)^bs * cs(i)

## where av,bv,cv,as,bs,cs are numbers governing the relative risks in alternative scenarios for victims and strikers, related to changes in distance/time at risk.

library(tidyverse)
load("Stats19.rda")

ss19<-tbl_df(ss19)
ss19<-filter(ss19, strike_mode!='No other vehicle')
ss19<-ss19[complete.cases(ss19),]

#adding age bands
ss19$cas_age_band= cut(ss19$cas_age, breaks = c(0, 20,40,60,80,110), right = FALSE)
ss19$strike_age_band= cut(ss19$strike_age, breaks = c(0, 16,25,60,80,110), right = FALSE)
table(ss19$cas_age_band, ss19$strike_age_band)

ss19 <- select(ss19, cas_mode, cas_severity, roadtype, strike_mode)

## Data under baseline scenario
ssg <-
  group_by(ss19, cas_mode, cas_severity, strike_mode, roadtype) %>% 
  summarise(count=n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_mode, cas_severity, strike_mode, roadtype, fill=list(count=0))

ssg.base <- ssg %>% 
  mutate(data_years = 7, # years over which the baseline data were collected
         ## Relative exposures/risks are 1 under the baseline scenario
         cas_reltime = 1,              # cv in the formula above
         strike_reltime = 1,           # cs 
         cas_reltime_group = 1,        # av
         strike_reltime_group = 1,     # as 
         ## power coefficients.  example values. These will actualy vary by group 
         sin_cas = 0.5,                # bv
         sin_strike = 0.7              # bs
         )

# Assume the observed number of injuries in the T-year period of the data in category i  is generated from a Poisson with rate (per T years)
## rateT(i) = T * rate(i), so that 
## log(rateT(i)) = log(T) + log(base_rate(i)) + log(cv(i)) + bv*log(av(i)) + log(cs(i)) + bs*log(as(i)) 

fit <- glm(count ~ 
             offset(log(data_years)) +    # log(T)
             cas_mode + cas_severity + strike_mode + roadtype +   # log(base_rate(i)).  Unknown coefficients, estimated by glm().  
             offset(log(cas_reltime)) +                           # log(cv(i))
             offset(log(strike_reltime)) +                        # log(cs(i))
             offset(I(sin_cas * log(cas_reltime_group))) +        # bv*log(av(i)) with bv known
             offset(I(sin_strike * log(strike_reltime_group))),   # bs*log(as(i)) with bs known
             data=ssg.base, family=poisson)

## Example data under alternative scenario with changed exposures/risks
## These will vary by group (real data not here yet)
ssg.scen1 <- ssg.base %>%
  mutate(data_years = 1,
         cas_reltime=2)

## Pass new scenario data to predict()
predbase <- predict(fit, newdata=ssg.base, se.fit=TRUE, type="response")
predscen1 <- predict(fit, newdata=ssg.scen1, se.fit=TRUE, type="response")

## check predictions 
predbase$fit / predscen1$fit # predicted counts 7/1 * 1/2 = 3.5 times as big in base case 

## arrange the predictions as you like...
ssg <- mutate(ssg,
              count_fit_base=predbase[["fit"]],
              count_se_base=predbase[["se.fit"]],
              count_fit_scen1=predscen1[["fit"]],
              count_se_scen1=predscen1[["se.fit"]]
              )
head(ssg, 10)
