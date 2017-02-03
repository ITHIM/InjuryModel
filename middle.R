
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
stopped$cas_severity = stopped$severitylab

# DATE
td = str_split(string = stopped$date,pattern = "/",n = 3, simplify = TRUE)
stopped$year = td[,3]
rm(td)

# recode ROAD CLASS
names(stopped)[4]= 'st_road_class'
stopped$st_road_class <-  recode(stopped$st_road_class,'1'=1, '2' = 1, '3'=2, '4'=3,'5'=3, '6'=3)    #1 stays unchanged

#add column roadtypelab
roadtypelab=data.frame(st_road_class=c(1,2,3), roadtypelab=c("Motorway/A(M)", "A","B, C, Unclassified"))
stopped = inner_join(stopped, roadtypelab, by="st_road_class")

stopped$roadtypelab = as.character(stopped$roadtypelab)   #to chr type
stopped$roadtype   =  stopped$roadtypelab

# MODE AND SEX OF VEHICLE AND CASUALTY
stopped$veh_mode = recode(stopped$vehicle_type,'-1'=99, '1'=2, '2'=3,'3'=3, '4'=3, '5'=3, '8'=4, '9'=4,
                              '10'=6, '11'=6, '16'=99, '17'=99,'18'=99, '19'=5, '20'=7, '21'=7, '22'=99,
                              '23'=3, '90'=99, '97'=3, '98'=7 ) 


stopped$cas_mode   = stopped$veh_mode
stopped$cas_mode[ stopped$casualty_class==3]   = 1
stopped$cas_mode[ is.na(stopped$casualty_class)]   = NA


##MODE AND SEX OF VEHICLE AND CASUALTY
modelab=data.frame(veh_mode=c(1,2,3,4,5,6,7,8,99), modelab=c("pedestrian","cyclist","motorcycle",
                                                    "car/taxi","light goods","bus","heavy goods",
                                                    "No other vehicle","other or unknown"))

stopped = inner_join(stopped, modelab, by='veh_mode')
stopped$modelab = as.character(stopped$modelab)

stopped$veh_mode = stopped$cas_mode = stopped$modelab 

stopped$cas_male = recode(stopped$sex_of_casualty, '-1'=NULL,'1' =1, '2'=0)
stopped$veh_male = recode(stopped$sex_of_driver, '-1'=NULL, '1'=1, '2'=0, '3' = NULL)

stopped$cas_age = recode(stopped$age_of_casualty, '-1'= NULL)
stopped$veh_age = recode(stopped$age_of_driver, '-1'= NULL)

#RENAMING FOR CONSISTENCY
stopped = dplyr::rename(stopped,  veh_reference  = vehicle_reference )


## NUMBER OF PEDESTRIANS, OF EACH SEX, IN ACCIDENT
stopped$pedflag = NULL
stopped$pedflag[stopped$cas_mode==1] = 1   # CHECK : 1 if cas_mode=1, 0, otherwise
stopped$pedflag[stopped$cas_mode!=1] = 0

## SELECT A 'STRIKE VEHICLE' PEDESTRIAN AT RANDOM 
## (NB ONLY KNOW ABOUT THOSE PEDESTRIANS WHO WERE INJURED...
## DON'T NEED TO SPLIT BY VEHICLE AS THIS ONLY BECOMES RELEVANT IF NO OTHER VEHICLE BUT THE PEDESTRIAN)

# check: add numped column
stopped= arrange(stopped, accident_index)
stopped.gr =  dplyr::group_by(.data = stopped,pedflag)
names(stopped.gr) = c('accident_index', 'numped')

stopped= inner_join(stopped, stopped.gr, by="accident_index")

# set seed 2010
stopped$random0 = runif(n = nrow(stopped),min = 0, max = 1)

## LITTLE N's
# foreach x in male age {
#   by accident_index cas_mode (random0), sort: gen littlen_cas`x'=_n
# 			gen ped_cas_`x'_temp=cas_`x'
# 			replace ped_cas_`x'_temp=. if cas_mode!=1 | littlen_cas`x'!=1  // pedestrian age/sex set as equal to one randomly selected pedestrian within the accident
# 			bysort accident_index: egen ped_cas_`x'=max(ped_cas_`x'_temp)
# 			drop littlen_cas`x' ped_cas_`x'_temp
# 			}
# 

## DEFINE LARGEST AND SECOND LARGEST OTHER VEHICLES, TO BECOME STRIKE VEHICLE
stopped = subset(stopped, select = c(accident_index,veh_mode,veh_reference,
                                     veh_male, veh_age, numped, ped_cas_male,
                                     ped_cas_age ))

# duplicates drop
stopped = stopped[!duplicated(stopped),]
stopped$veh_modei=- stopped$veh_mode
stopped$veh_modei[stopped$veh_modei == -99] = 99

stopped$random1 = runif(n = nrow(stopped),min = 0, max = 1)

## another little_n !!
# by accident_index (veh_modei random1), sort: gen littlen=_n
# keep accident_index veh_reference veh_mode veh_male veh_age littlen numped ped_cas_male ped_cas_age  
# reshape wide veh_reference veh_mode veh_male veh_age, i(accident_index) j(littlen)
# foreach x in reference mode male age {
#   rename veh_`x'1 veh_`x'_firstlarge
# 			rename veh_`x'2 veh_`x'_secondlarge
# 			}

stopped = subset(x = stopped, select = c(accident_index,veh_reference, veh_mode,
                                         veh_male, veh_age, littlen, numped, 
                                         ped_cas_male, ped_cas_age))

stopped = reshape

# #create male flag
# stopped$malepedflag=NULL
# stopped$malepedflag[stopped$cas_mode==1 & stopped$cas_male==1] = 1
# stopped.gr = base::aggregate(stopped$malepedflag, data=stopped , FUN=sum)   ##ERROR
# names(stopped.gr)=c('a', 'b')       ## !!
# 
# #create female flag
# stopped$femalepedflag=NULL
# stopped$femalepedflag[stopped$cas_mode==1 & stopped$cas_male==0] = 1
# stopped.gr = base::aggregate(stopped$femalepedflag, data=stopped , FUN=sum)
# 
# names(stopped.gr)=c('a', 'numfemaleped')
# 
# ##inner_join()
# 
# drop=c('pedflag','malepedflag', 'femalepedflag')
# stopped = stopped[, ! (names(stopped) %in% drop) ]

# ## save as .intermediate .CSV file 


