
rm(list=ls())

library(stringr)
library(readstata13)
library(dplyr)

stopped = readstata13::read.dta13('stopped.dta')
stopped = stopped[, c(1:12)]   # clean odd columns errors

stopped = dplyr::rename(stopped,  cas_severity = casualty_severity )
severitylab=data.frame(cas_severity=c(1,2,3), severitylab=c("Fatal","Serious","Slight"))

# adds severity lab column (WATCH OUT NA LINES)
stopped=inner_join(stopped, severitylab, by="cas_severity")
stopped$severitylab = as.character(stopped$severitylab)   # to char type

# DATE
td = str_split(string = stopped$date,pattern = "/",n = 3, simplify = TRUE)
stopped$year = td[,3]
rm(td)

# recode ROAD CLASS
stopped$st_road_class <-  recode(stopped$st_road_class,'1'=1, '2' = 1, '3'=2, '4'=3,'5'=3, '6'=3)    #1 stays unchanged

#add column roadtypelab
roadtypelab=data.frame(st_road_class=c(1,2,3), roadtypelab=c("Motorway/A(M)", "A","B, C, Unclassified"))
stopped = inner_join(stopped, roadtypelab, by="st_road_class")

stopped$roadtypelab = as.character(stopped$roadtypelab)   #to chr type

# MODE AND SEX OF VEHICLE AND CASUALTY
stopped$veh_mode = recode(stopped$vehicle_type,'-1'=99, '1'=2, '2'=3,'3'=3, '4'=3, '5'=3, '8'=4, '9'=4,
                              '10'=6, '11'=6, '16'=99, '17'=99,'18'=99, '19'=5, '20'=7, '21'=7, '22'=99,
                              '23'=3, '90'=99, '97'=3, '98'=7 ) 


stopped$cas_mode   = stopped$veh_mode
stopped$cas_mode[ stopped$casualty_class==3]   = 1
stopped$cas_mode[ is.na(stopped$casualty_class)]   = NA

modelab=data.frame(veh_mode=c(1,2,3,4,5,6,7,8,99), modelab=c("pedestrian","cyclist","motorcycle",
                                                    "car/taxi","light goods","bus","heavy goods",
                                                    "No other vehicle","other or unknown"))


stopped = inner_join(stopped, modelab, by='veh_mode')
stopped$modelab = as.character(stopped$modelab)

stopped$veh_mode = stopped$cas_mode = stopped$modelab 

stopped$cas_male = recode(stopped$sex_of_casualty, '-1'=NULL,'1' =1, '2'=0) 
stopped$veh_male = recode(stopped$sex_of_driver, '-1'=NULL, '1'=1, '2'=0, '3' = NULL)

## NUMBER OF PEDESTRIANS, OF EACH SEX, IN ACCIDENT
stopped$pedflag = NULL
stopped$pedflag[stopped$cas_mode==1] = 1   # CHECK : 1 if cas_mode=1, 0, otherwise
stopped$pedflag[stopped$cas_mode!=1] = 0


# check 
stopped= arrange(stopped, accident_index)
stopped.gr =  dplyr::group_by(.data = stopped,pedflag)

stopped= inner_join(stopped, stopped.gr, by="accident_index")

#create male flag
stopped$malepedflag=NULL
stopped$malepedflag[stopped$cas_mode==1 & stopped$cas_male==1] = 1
stopped.gr = base::aggregate(stopped$malepedflag, data=stopped , FUN=sum)
names(stopped.gr)=c('a', 'b')       ## !!

#create female flag
stopped$femalepedflag=NULL
stopped$femalepedflag[stopped$cas_mode==1 & stopped$cas_male==0] = 1
stopped.gr = base::aggregate(stopped$femalepedflag, data=stopped , FUN=sum)

names(stopped.gr)=c('a', 'numfemaleped')

##inner_join()

drop=c('pedflag','malepedflag', 'femalepedflag')
stopped = stopped[, ! (names(stopped) %in% drop) ]

# ## save as .intermediate .CSV file 
















