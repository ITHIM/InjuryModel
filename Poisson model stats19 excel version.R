#install.packages("tidyr")
#install.packages("dplyr")

library(readstata13)
library(dplyr)
library(tidyr)

#reading in the data
#ss19<-read.dta13("C:/Users/jw745/Dropbox/z_ITHIMfiles/Stats19/1b_DataCreated/Stats19_05-15_ready_v3.dta")
ss19<-tbl_df(ss19)
ss19<-filter(ss19, strike_mode!='No other vehicle')
ss19<-ss19[complete.cases(ss19),]
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
ss19<-select(ss19,"cas_mode", "cas_severity", "roadtype", "strike_mode")
ssg <- group_by(ss19, cas_mode, cas_severity, strike_mode, roadtype) %>% 
  #  select (ss19, cas_mode, cas_male, cas_severity, strike_mode, cas_age_band, strike_age_band, roadtype, strike_male) %>% 
  summarise(count=n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_mode, cas_severity, strike_mode, roadtype, fill=list(count=0))

#this is Excel ithim 1 structure

fit <-  glm(count ~  (cas_mode *cas_severity* roadtype* strike_mode), data=ssg, family=poisson)
      # add predicted count with standard error 
pred <- predict(fit, se.fit=TRUE, type="response")
ssg <- mutate(ssg, count_fit=pred[["fit"]], count_se=pred[["se.fit"]])
as.data.frame(ssg)[1:10,]

