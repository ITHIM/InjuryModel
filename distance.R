## Demo of how to do alternative scenarios in Poisson model 

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
  mutate(data_years = 7,
         cas_reltime = 1,  # all below should vary by group... 
         strike_reltime = 1,
         cas_reltime_group = 1,  
         strike_reltime_group = 1,
         ## power coefficients.  These will actualy vary by group 
         sin_cas = 0.5,
         sin_strike = 0.7
         )

fit <- glm(count ~ 
             cas_mode + cas_severity + strike_mode + roadtype +
             offset(log(data_years)) +
             offset(log(cas_reltime)) + 
             offset(log(strike_reltime)) + 
             offset(I(sin_cas * log(cas_reltime_group))) + 
             offset(I(sin_strike * log(strike_reltime_group))),
             data=ssg.base, family=poisson)

## Data under alternative scenario
ssg.scen1 <- ssg.base %>%
  mutate(data_years = 1,
         cas_reltime=2)

predbase <- predict(fit, newdata=ssg.base, se.fit=TRUE, type="response")
predscen1 <- predict(fit, newdata=ssg.scen1, se.fit=TRUE, type="response")

predbase$fit / predscen1$fit # predicted counts 7/1 * 1/2 = 3.5 times as big in base case 

ssg <- mutate(ssg,
              count_fit_base=predbase[["fit"]],
              count_se_base=predbase[["se.fit"]],
              count_fit_scen1=predscen1[["fit"]],
              count_se_scen1=predscen1[["se.fit"]]
              )
head(ssg, 10)
