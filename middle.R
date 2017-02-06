
# carries on processing object: stopped  

#deletes legacy objects
rm(Accidents0515,av,avc, Casualties0515,Vehicles0515)

library(stringr)
library(readstata13)
library(dplyr)
library(stats)

stopped = read.csv('stopped.csv', header=T)
#stopped = readstata13::read.dta13('stopped.dta')    #only in casestopped object is not downloaded
#stopped = stopped[, c(1:14)]   # clean odd columns errors

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

stopped$cas_mode   = stopped$veh_mode  
stopped$cas_mode[ stopped$casualty_class==3]   = 1
stopped$cas_mode[ is.na(stopped$cas_severity)]   = NA

modelab=data.frame(veh_mode=c(1,2,3,4,5,6,7,8,99), modelab=c("pedestrian","cyclist","motorcycle",
                                                    "car/taxi","light goods","bus","heavy goods",
                                                    "No other vehicle","other or unknown"))

stopped = inner_join(stopped, modelab, by='veh_mode')
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


## NUMBER OF PEDESTRIANS, OF EACH SEX, IN ACCIDENT    ???  check!!
stopped$pedflag = NA   
stopped$pedflag[stopped$cas_mode==1] = 1   # CHECK : 1 if cas_mode=1, 0, otherwise
stopped$pedflag[stopped$cas_mode!=1] = 0

## SELECT A 'STRIKE VEHICLE' PEDESTRIAN AT RANDOM 
## (NB ONLY KNOW ABOUT THOSE PEDESTRIANS WHO WERE INJURED...
## DON'T NEED TO SPLIT BY VEHICLE AS THIS ONLY BECOMES RELEVANT IF NO OTHER VEHICLE BUT THE PEDESTRIAN)

# check: add numped column
stopped= arrange(stopped, accident_index)
stopped.gr = aggregate(stopped$pedflag, by =list(stopped$accident_index), FUN=sum, na.rm=T)
# alternative: dplyr::group_by(.data = stopped, pedflag)
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
stopped1 = subset(stopped, select = c(accident_index,veh_mode,veh_reference,
                                     veh_male, veh_age, numped, ped_cas_male,
                                     ped_cas_age ))

# duplicates drop
stopped1 = stopped1[!duplicated(stopped1),]
stopped1$veh_modei=- stopped1$veh_mode
stopped1$veh_modei[stopped1$veh_modei == -99] = 99

stopped1$random1 = runif(n = nrow(stopped1),min = 0, max = 1)

## another little_n !!
# by accident_index (veh_modei random1), sort: gen littlen=_n
# keep accident_index veh_reference veh_mode veh_male veh_age littlen numped ped_cas_male ped_cas_age  
# reshape wide veh_reference veh_mode veh_male veh_age, i(accident_index) j(littlen)
# foreach x in reference mode male age {
#   rename veh_`x'1 veh_`x'_firstlarge
# 			rename veh_`x'2 veh_`x'_secondlarge
# 			}

stopped1 = subset(x = stopped1, select = c(accident_index,veh_reference, veh_mode,
                                         veh_male, veh_age, littlen, numped, 
                                         ped_cas_male, ped_cas_age))

#! stopped1 = reshape(data = stopped1,       ********)

stopped1=rename(.data = stopped1,   veh_mode_firstlarge = veh_mode,
                                  veh_male_firstlarge = veh_male,
                                  veh_age_firstlarge = veh_age,
                                  veh_mode_secondlarge = veh_mode,
                                  veh_male_secondlarge = veh_male,
                                  veh_age_secondlarge = veh_age)


stopped1$veh_mode_secondlarge[is.na(stopped1$veh_mode_secondlarge) & stopped1$numped!= 0 ] =  1
stopped1$veh_mode_secondlarge[is.na(stopped1$veh_mode_secondlarge)] = 8


stopped1$veh_male_secondlarge[stopped1$numped!=0 & stopped1$veh_mode_secondlarge == 1] = stopped1$ped_cas_male
stopped1$veh_age_secondlarge[stopped1$numped!=0 & stopped1$veh_mode_secondlarge == 1] = stopped1$ped_cas_age

stopped1 = subset(stopped1, select =c(accident_index, veh_reference_firstlarge, veh_reference_secondlarge,
                                    veh_mode_firstlarge, veh_mode_secondlarge, veh_male_firstlarge,
                                    veh_male_secondlarge, veh_age_firstlarge, veh_age_secondlarge)   )


write.csv(stopped1, file = './stats19_strikemode.dta')

stopped = inner_join(stopped, stopped1, by="accident_index")

i= c('mode','male','age')

for (x in i) {
  stopped[[paste0('strike_', i) ]]= NULL
  stopped[[paste0('strike_', i) ]][stopped$cas_mode==1] = stopped[[paste0('veh_', i) ]]     #if cas_mode==1
  stopped[[paste0('strike_', i, '_secondlarge') ]][stopped$veh_reference== stopped$veh_reference_firstlarge
                                                   & stopped$cas_mode != 1] = stopped[[paste0('veh_', i, '_secondlarge') ]]
              }  
  

stopped$modelab = stopped$strike_mode

#IMPUTE AT RANDOM MISSING SEX OF A) CASUALTY AND B) STRIKER, 
# IN PROPORTION TO OBSERVED SEX RATIO OF STRIKER COLLISIONS FOR EACH MODE [not done for age]

stopped$random3 = runif(n = nrow(stopped), min = 0, max = 1)
table( stopped$cas_male, stopped$miss)   # missing data 0.2% casualty sex

table( stopped$cas_male, stopped$miss) 
table(stopped$strike_male[stopped$strike_mode!=8] , miss )  # missing data 6.5% striker sex

for (x in c('cas', 'strike')) {
  stopped[[paste0(x,'_mode_sexratio')]]= NULL
			
  for (i in c(1:7,99) ) {
			temp.mean = mean( stopped[[paste0(x, '_male']] [paste(x, '_mode')== i]  #the mean in the summary
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
stopped = stopped[! is.na(stopped$cas_severity)]
stopped = arrange(stopped, accident_index, year, roadtype, cas_severity, cas_mode,
                  cas_male, cas_age, strike_mode, strike_male, strike_age)

#save a range of columns
ncol1=which(names(stopped)=='accident_index')
ncol2=which(names(stopped)=='strike_age')

stopped = stopped [, c(ncol1:ncol2) ]

for (i in 1:names(stopped)) {
  names(stopped)[i]=paste0('var', names(stopped)[i])
                            }

write.csv(stopped, './1b_DataCreated/stats19_05-15_ready_v3.csv')



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