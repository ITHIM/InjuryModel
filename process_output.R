
#JW code to transform output

library(tidyverse)
output<-read.dta13("C:/Users/jw745/Dropbox/z_ITHIMfiles/Stats19/1b_DataCreated/Stats19_05-15_ready_v3.dta")
output <- group_by(output, cas_mode, cas_severity, strike_mode, roadtype) %>%
  summarise(count=n()) %>%
  droplevels() %>%
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_mode, cas_severity, strike_mode, roadtype, fill=list(count=0)) %>%
  spread (cas_mode, count)%>%
  arrange(cas_severity, roadtype, strike_mode)
select(output, one_of(c("cas_severity", "roadtype", "strike_mode","pedestrian", "cyclist", "bus", 'car/taxi', "heavy goods", "light goods", "motorcycle", "No other vehicle")))