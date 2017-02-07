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
ssg <- group_by(ss19, cas_mode, cas_severity, strike_mode, roadtype) %>% 
  summarise(count=n()) %>% 
  droplevels() %>% 
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_mode, cas_severity, strike_mode, roadtype, fill=list(count=0))

#this is Excel ithim 1 structure
fitsat <- glm(count ~ cas_mode*cas_severity*strike_mode*roadtype, data=ssg, family=poisson)
pred <- predict(fitsat, se.fit=TRUE, type="response")
ssg <- mutate(ssg, count_fit_sat=pred[["fit"]], count_se_sat=pred[["se.fit"]])
as.data.frame(ssg)[1:10,]