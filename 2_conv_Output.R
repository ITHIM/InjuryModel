
#converts stats19_05-15 to the ITHIM model

library(tidyverse)

stats19_0515 <- readRDS("./1b_DataCreated/stats19_05-15_ready_v3.Rds")


#limit casualties to Serious/Fatal
stats19_0515 = stats19_0515[stats19_0515$cas_severity!='Slight', ]
stats19_0515 = stats19_0515[stats19_0515$strike_mode.int!=8, ]    # no other vehicle discarded

#recode & rename strike mode to ITHIM categories
stats19_0515$strike_mode = recode(stats19_0515$strike_mode.int, '1'="walk",'2' ="cycle",'3'="mbike",
                                  '4'="car",'5'="LGV",'6'="bus",'7'="HGV", '9'= 'other',
                                  '8' = "NOV", '99' ="other or unknown")

stats19_0515$cas_mode = recode(stats19_0515$cas_mode.int, '1'="walk",'2' ="cycle",'3'="mbike",
                                  '4'="car",'5'="LGV",'6'="bus",'7'="HGV",
                                  '8' = "NOV", '99' ="other or unknown")



stats19_0515$roadtype = recode(stats19_0515$roadtype, 'Motorway/A(M)'="Highway",
                                  'A' ="Arterial",'B, C, Unclassified'="Local")

                                   
output <- group_by(stats19_0515, cas_severity, roadtype, strike_mode, cas_mode) %>% 
       summarise(count=n()) %>% 
       droplevels() %>% 
       as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
       complete(cas_severity, roadtype, strike_mode, cas_mode, fill=list(count=0)) %>%
       arrange(cas_severity, roadtype, strike_mode)  


output <- as.data.frame(output)
output <- output[output$strike_mode !='other or unknown',]
output$cas_mode[output$cas_mode=='other or unknown']='NOV'   #check this assumption

output = dplyr::rename(output, severity =cas_severity ,
                               roadType  = roadtype   ,
                               strikingMode = strike_mode,
                               victimMode = cas_mode  ,
                               value = count     )
                               
write.csv(output, file="roadInjuries.csv", quote=FALSE, row.names=FALSE)
