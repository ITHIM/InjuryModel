# carries on processing object: stopped  
rm(list=ls())
#deletes legacy objects
rm(Accidents0515,av,avc, Casualties0515,Vehicles0515)

library(stringr)
library(readstata13)
library(dplyr)
library(stats)
library(tidyr)

stopped = read.csv('stopped.csv', header=T)
#stopped = readstata13::read.dta13('stopped.dta')    #only in casestopped object is not downloaded
#stopped = stopped[, c(1:14)]   # clean odd columns errors
# load('stopped.rda')

stopped = dplyr::rename(stopped,  cas_severity = casualty_severity )
severitylab=data.frame(cas_severity=c(1,2,3), severitylab=c("Fatal","Serious","Slight"))

# adds severity lab column 
stopped = inner_join(stopped, severitylab, by="cas_severity")
stopped$severitylab = as.character(stopped$severitylab)   # to char type
stopped$cas_severity = stopped$severitylab

# DATE
td = str_split(string = stopped$date,pattern = "/",n = 3, simplify = TRUE)
stopped$year = td[,3]
rm(td)

# rename ROAD CLASS
names(stopped)[which(names(stopped)=='x1st_road_class')]= 'st_road_class'
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

#keep integer values for stopped1$veh_mode
stopped$veh_mode.int = stopped$veh_mode 

stopped$cas_mode.int   = stopped$veh_mode.int  
stopped$cas_mode.int[stopped$casualty_class==3]   = 1
stopped$cas_mode.int[ is.na(stopped$cas_severity)]   = NA

modelab=data.frame(veh_mode.int=c(1,2,3,4,5,6,7,8,99), modelab=c("pedestrian","cyclist","motorcycle",
                                                    "car/taxi","light goods","bus","heavy goods",
                                                    "No other vehicle","other or unknown"))

stopped = inner_join(stopped, modelab, by='veh_mode.int')
stopped$modelab = as.character(stopped$modelab)

 
stopped$veh_mode = stopped$cas_mode = stopped$modelab   #CAUTION: integers replaced by labels !!

stopped$cas_male = recode(stopped$sex_of_casualty, '-1'=NULL,'1' =1, '2'=0)
stopped$veh_male = recode(stopped$sex_of_driver, '-1'=NULL, '1'=1, '2'=0, '3' = NULL)

#check totals
stopped$cas_age = stopped$age_of_casualty    #replicate variable
stopped$cas_age[stopped$stopped$cas_age== -1] = NA   
stopped$veh_age = stopped$age_of_driver
stopped$veh_age[stopped$veh_age== -1 ] = NA

#RENAMING FOR CONSISTENCY
stopped = dplyr::rename(stopped,  veh_reference  = vehicle_reference )


## SELECT A 'STRIKE VEHICLE' PEDESTRIAN AT RANDOM 
## (NB ONLY KNOW ABOUT THOSE PEDESTRIANS WHO WERE INJURED...
## DON'T NEED TO SPLIT BY VEHICLE AS THIS ONLY BECOMES RELEVANT IF NO OTHER VEHICLE BUT THE PEDESTRIAN)

## NUMBER OF PEDESTRIANS, OF EACH SEX, IN ACCIDENT    
stopped$pedflag = NA   
stopped$pedflag[stopped$cas_mode.int==1] = 1   # CHECK : 1 if cas_mode=1, 0, otherwise
stopped$pedflag[stopped$cas_mode.int!=1] = 0


# check: add numped column
stopped= arrange(stopped, accident_index)
stopped.gr = aggregate(stopped$pedflag, by =list(stopped$accident_index), FUN=sum, na.rm=T)
names(stopped.gr) = c('accident_index', 'numped')

stopped= inner_join(stopped, stopped.gr, by="accident_index")

# set seed 2010
set.seed(2010)
stopped$random0 = runif(n = nrow(stopped),min = 0, max = 1)

td= stopped

## LITTLE N's
for (x in c('male', 'age')) {

   by_td <- td %>% group_by(accident_index, cas_mode.int)   # groups by 2 vars
   td <- mutate(arrange(td,accident_index, cas_mode.int,random0),vartemp=unlist(lapply(group_size(by_td),FUN=seq_len)))    # sorts by 3 vars->generate index
   
    td[[paste0('littlen_cas', x) ]] = td$vartemp   ; td$vartemp =NULL
   
		td[[paste0('ped_cas_', x) ]] = td[[ paste0('cas_', x) ]]
		
		td[[ paste0('ped_cas_', x) ]][ td$cas_mode.int!=1 | td[[paste0('littlen_cas', x) ]]!=1  ] = NA
			
    #replace ped_cas_`x'_temp=. if cas_mode!=1 | littlen_cas`x'!=1  // pedestrian age/sex set as equal to one randomly selected pedestrian within the accident
		#td[[ paste0('ped_cas_', x, '_temp') ]] 
		
    #bysort accident_index: egen ped_cas_`x'=max(ped_cas_`x'_temp)
		vartemp = paste0('littlen_cas', x)
		td.gr  = aggregate(td[[vartemp]], by = list(td$accident_index), FUN = max, na.rm=TRUE)
		names(td.gr) = c('accident_index', paste0('ped_cas_', x,'_temp1'))
		td  = inner_join(td, td.gr, by= 'accident_index')
		
		td[[ paste0('ped_cas_', x) ]] = td[[ paste0('ped_cas_', x,'_temp1') ]] 
		td[[ paste0('ped_cas_', x,'_temp1')]]  = NULL
		
	  td[[paste0('littlen_cas', x)]] = NULL
    #drop littlen_cas`x' ped_cas_`x'_temp
	  
			}

# #stopped = readstata13::read.dta13('predefine.dta'); stopped=stopped[,c(1:30)]
# stopped=inner_join(stopped,td[,c("accident_index","date", "veh_mode.int")],
# by=c("accident_index"="accident_index", "date"="date") )

#remove loop intermediate components & collect
rm(td.gr, stopped.gr)
stopped= td ; rm(td)

## DEFINE LARGEST AND SECOND LARGEST OTHER VEHICLES, TO BECOME STRIKE VEHICLE
stopped1 = subset(stopped, select = c(accident_index,veh_mode,veh_mode.int, veh_reference,
                                     veh_male, veh_age, numped, ped_cas_male,
                                     ped_cas_age ))

# duplicates drop
stopped1 = stopped1[!duplicated(stopped1),]   #likely no duplicates
stopped1$veh_modei = -1 *stopped1$veh_mode.int
stopped1$veh_modei[stopped1$veh_modei == -99] = 99

set.seed(2011)
stopped1$random1 = runif(n = nrow(stopped1),min = 0, max = 1)

## another little_n !!   CHECK
# by accident_index (veh_modei random1), sort: gen littlen=_n
by_stopped1 <- stopped1 %>% group_by(accident_index)   # groups by
stopped1 <- mutate(arrange(stopped1, accident_index, veh_modei, random1),littlen=unlist(lapply(group_size(by_stopped1),FUN=seq_len)))    # sorts by 3 vars->generate index


stopped1 = subset(x = stopped1, select = c(accident_index,veh_reference, veh_mode,
                                         veh_male, veh_age, littlen, numped, 
                                         ped_cas_male, ped_cas_age)   )
# keep accident_index veh_reference veh_mode veh_male veh_age littlen numped ped_cas_male ped_cas_age  

# !! reshape wide veh_reference veh_mode veh_male veh_age, i(accident_index) j(littlen)
stopped1 = reshape(data = stopped1,v.names = c('veh_reference','veh_mode','veh_male','veh_age'),
                           timevar='littlen' , idvar = c('accident_index')   ,  direction = "wide")

for (x in c('reference','mode','male','age')) {
#      stopped1 = dplyr::rename(stopped1, paste0('veh_', x, '_firstlarge') = paste0('veh_', x)  )
#			stopped1 = dplyr::rename(stopped1, paste0('veh_',x,'2')= paste0('veh_',x.'_secondlarge'))
      names(stopped1)[names(stopped1)==paste0('veh_',x,'.1')] = paste0('veh_',x,'_firstlarge')
      names(stopped1)[names(stopped1)==paste0('veh_',x,'.2')] = paste0('veh_',x,'_secondlarge')
			}


# one n loop
#stopped1=rename(.data = stopped1,   veh_mode_firstlarge = veh_mode,
#                                   veh_male_firstlarge = veh_male,
#                                   veh_age_firstlarge = veh_age,
#                                   veh_mode_secondlarge = veh_mode,
#                                   veh_male_secondlarge = veh_male,
#                                   veh_age_secondlarge = veh_age)


stopped1$veh_mode_secondlarge[is.na(stopped1$veh_mode_secondlarge) & stopped1$numped!= 0 ] =  1
stopped1$veh_mode_secondlarge[is.na(stopped1$veh_mode_secondlarge)] = 8

sel = stopped1$numped!=0 & stopped1$veh_mode_secondlarge == 1
stopped1$veh_male_secondlarge[sel] = stopped1$ped_cas_male[sel]

sel= stopped1$numped!=0 & stopped1$veh_mode_secondlarge == 1
stopped1$veh_age_secondlarge[sel] = stopped1$ped_cas_age[sel]

stopped1 = subset(stopped1, select =c(accident_index, veh_reference_firstlarge, veh_reference_secondlarge,
                                    veh_mode_firstlarge, veh_mode_secondlarge, veh_male_firstlarge,
                                    veh_male_secondlarge, veh_age_firstlarge, veh_age_secondlarge)   )


write.csv(stopped1, file = './stats19_strikemode.csv')

stopped = inner_join(stopped, stopped1, by="accident_index")


for (x in c('mode','male','age')) {

  stopped[[paste0('strike_', x) ]]= NA
  sel= stopped$cas_mode==1
  stopped[[paste0('strike_', x) ]][sel] = stopped[[paste0('veh_', x) ]][sel]    # 1 if cas_mode==1, 0 otherwise
  
  sel= stopped$cas_mode!=1
  stopped[[paste0('strike_', x) ]][sel] = stopped[[paste0('veh_', x,'_firstlarge') ]][sel]     
  
  sel= (stopped$veh_reference== stopped$veh_reference_firstlarge  & stopped$cas_mode != 1)
  stopped[[paste0('strike_', x) ]][ sel ] = stopped[[paste0('veh_', x, '_secondlarge') ]][sel]
              }  
  

stopped$modelab = stopped$strike_mode  #check integer/labels OK

#IMPUTE AT RANDOM MISSING SEX OF A) CASUALTY AND B) STRIKER, 
# IN PROPORTION TO OBSERVED SEX RATIO OF STRIKER COLLISIONS FOR EACH MODE [not done for age]
set.seed(2012)
stopped$random3 = runif(n = nrow(stopped), min = 0, max = 1)
table( stopped$cas_male, useNA = "always")   # missing data 0.2% casualty sex

table(stopped$strike_male[stopped$strike_mode!=8] , useNA = "always" )  # missing data 6.5% striker sex

for (x in c('cas', 'strike')) {
  stopped[[paste0(x,'_mode_sexratio')]]= NA
			
  for (i in c(1:7,99) ) {
			temp.mean = mean( stopped[[paste0(x, '_male')]][paste(x, '_mode')== i])  #the mean in the summary
			stopped[[ paste0(x, '_mode_sexratio')]][paste(x, '_mode')== i]=  temp.mean
		                    }
			                }

sel= is.na(stopped$cas_male) & (stopped$random3 <= stopped$cas_mode_sexratio) & !is.na(stopped$cas_severity) 
stopped$cas_male[ sel ] = 1 

sel= is.na(stopped$cas_male) & (stopped$random3 > stopped$cas_mode_sexratio) & !is.na(stopped$cas_severity)
stopped$cas_male[ sel ] = 0

sel= is.na(stopped$strike_male) & (stopped$random3 <= stopped$cas_mode_sexratio) & stopped$strike_mode != 8
stopped$strike_male[ sel ] = 1 

sel= is.na(stopped$strike_male) & (stopped$random3 > stopped$cas_mode_sexratio) & stopped$strike_mode != 8
stopped$strike_male[ sel ] = 0

#SAVE
stopped = stopped[! is.na(stopped$cas_severity),]
stopped = arrange(stopped, accident_index, year, roadtype, cas_severity, cas_mode,
                  cas_male, cas_age, strike_mode, strike_male, strike_age)

#save a range of columns
ncol1= which(names(stopped)=='accident_index')
ncol2= which(names(stopped)=='strike_age')

stopped = stopped [, c(ncol1:ncol2) ]

for (i in (1:length(names(stopped))))  {
  names(stopped)[i] = as.char(paste0('var', names(stopped)[i]))
                            }

write.csv(stopped, './1b_DataCreated/stats19_05-15_ready_v3.csv')
saveRDS(stopped, './1b_DataCreated/stats19_05-15_ready_v3.Rds')


# ***************************
#   * ANALYSIS
# ***************************

# use "1b_DataCreated\Stats19_05-15_ready.dta", clear
# recode strike_mode 3=5 5=6 6=3 7=7 8=9 99=8, gen(strike_modecat)
# recode cas_mode 3=5 5=6 6=3 7=7 8=9 99=8, gen(cas_modecat)
# label define modecatlab 1 "walk" 2 "cycle" 3 "bus" 4 "car" 5 "mbike" 6 "van" 7 "lorry" 8 "other" 9 "no other vehicle" , modify
# label values strike_modecat modecatlab
# label values cas_modecat modecatlab
# 
# * CASUALTY PERSPECTIVE (only use pedestrian, cyclist, motorcycle, car/taxi)
# foreach i in 1 0 {
#   foreach j in 1 2 3 {
#     bysort cas_severity: tab cas_modecat strike_modecat if cas_male==`i' & roadtype==`j'
#   }
#   }
#     
#     * STRIKER PERSPECTIVE
#     foreach i in 1 0 {
#     foreach j in 1 2 3 {
#     bysort cas_severity: tab strike_modecat cas_modecat if strike_male==`i' & roadtype==`j'
#     }
#     }
#     
