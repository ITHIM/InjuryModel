
#JW code to transform output into the ITHIM model

library(tidyverse)
stats19_0515 <- readRDS("stats19_05-15_ready_v3.Rds")
output <- group_by(stats19_0515, cas_mode, cas_severity, strike_mode, roadtype) %>%
  summarise(count=n()) %>%
  droplevels() %>%
  as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
  complete(cas_mode, cas_severity, strike_mode, roadtype, fill=list(count=0)) %>%
  spread (cas_mode, count)%>%
  arrange(cas_severity, roadtype, strike_mode)
select(output, one_of(c("cas_severity", "roadtype", "strike_mode","pedestrian", "cyclist", "bus", 'car/taxi', "heavy goods", "light goods", "motorcycle", "other or unknown")))
