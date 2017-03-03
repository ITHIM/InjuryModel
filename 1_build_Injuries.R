# carries on processing object: stopped  

#deletes legacy objects / rm(list=ls())
rm(Accidents0515,av,avc, Casualties0515,Vehicles0515)

library(stringr)
library(readstata13)
library(dplyr)
library(stats)
library(tidyr)

load('stopped.rda')
stopped = stopped[, c(1:14)]   # clean odd columns errors

# PREPARE VARIABLES
stopped = dplyr::rename(stopped,  cas_severity = casualty_severity )
stopped$cas_severity = recode(stopped$cas_severity, '1'="Fatal",'2'="Serious", '3'="Slight")

# DATE
td = str_split(string = stopped$date, pattern = "/",n = 3, simplify = TRUE)
stopped$year = td[,3]
rm(td)

# rename ROAD CLASS
stopped$st_road_class =  recode(stopped$st_road_class,'1'=1,'2' = 1, '3'=2, '4'=3,'5'=3, '6'=3) #1 unchanged
stopped$roadtype = recode(stopped$st_road_class, '1'="Motorway/A(M)", '2'="A", '3'="B, C, Unclassified") 

# MODE AND SEX OF VEHICLE AND CASUALTY
stopped$veh_mode = recode(stopped$vehicle_type,'-1'=99, '1'=2, '2'=3,'3'=3, '4'=3, '5'=3, '8'=4, '9'=4,
                              '10'=6, '11'=6, '16'=99, '17'=99,'18'=99, '19'=5, '20'=7, '21'=7, '22'=99,
                              '23'=3, '90'=99, '97'=3, '98'=7 ) 

#keep integer values for future
stopped$veh_mode.int = stopped$cas_mode.int = stopped$veh_mode 

stopped$cas_mode.int[stopped$casualty_class==3]   = 1
stopped$cas_mode.int[ is.na(stopped$cas_severity)]   = NA

#creates veh_mode/cas_mode label vars
stopped$veh_mode = recode(stopped$veh_mode.int, '1'="pedestrian",'2' ="cyclist",'3'="motorcycle",
                                    '4'="car/taxi",'5'="light goods",'6'="bus",'7'="heavy goods",
                                    '8' = "NOV", '99' ="other or unknown")

stopped$cas_mode = recode(stopped$cas_mode.int, '1'="pedestrian",'2' ="cyclist",'3'="motorcycle",
                                '4'="car/taxi",'5'="light goods",'6'="bus",'7'="heavy goods",
                                '8' = "NOV", '99' ="other or unknown")

#sex of casualty
stopped$cas_male = recode(stopped$sex_of_casualty, '-1'=NULL,'1' =1, '2'=0)
stopped$veh_male = recode(stopped$sex_of_driver, '-1'=NULL, '1'=1, '2'=0, '3' = NULL)

#ages
stopped$cas_age = stopped$age_of_casualty    #replicate variable
stopped$cas_age[stopped$cas_age== -1] = NA   
stopped$veh_age = stopped$age_of_driver
stopped$veh_age[stopped$veh_age== -1 ] = NA

#RENAMING FOR CONSISTENCY
stopped = dplyr::rename(stopped,  veh_reference  = vehicle_reference )


############### START ALLOCATION ALGORITHM

## SELECT A 'STRIKE VEHICLE' PEDESTRIAN AT RANDOM 
## (NB ONLY KNOW ABOUT THOSE PEDESTRIANS WHO WERE INJURED...
## DON'T NEED TO SPLIT BY VEHICLE AS THIS ONLY BECOMES RELEVANT IF NO OTHER VEHICLE BUT THE PEDESTRIAN)

## NO. OF PEDESTRIANS IN ACCIDENT    
stopped$pedflag = 0   
stopped$pedflag[stopped$cas_mode.int==1] = 1   #  1 if cas_mode=1 | 0: otherwise

# add "numped" column
stopped= arrange(stopped, accident_index)
stopped.gr = aggregate(stopped$pedflag, by =list(stopped$accident_index), FUN=sum, na.rm=T)
names(stopped.gr) = c('accident_index', 'numped')

stopped= inner_join(stopped, stopped.gr, by="accident_index")

# set seed 2010
set.seed(2010)
stopped$random0 = runif(n = nrow(stopped), min = 0, max = 1)

#used in next loop
by_stopped <- stopped %>% group_by(accident_index, cas_mode.int)   # groups by 2 vars

## LITTLE N's 
for (x in c('male', 'age')) {

   # sorts by 3 vars->generate little_n's, delete intermediate var
   stopped <- mutate(arrange(stopped,accident_index, cas_mode.int, random0),
                     vartemp=unlist(lapply(group_size(by_stopped), FUN=seq_len)))
   stopped[[paste0('littlen_cas', x) ]] = stopped$vartemp   ; stopped$vartemp =NULL

   #pedestrians= casualties hurt in mode=1    
   #pedestrian age/sex set equal to one randomly selected pedestrian within the accident
	 stopped[[paste0('ped_cas_', x) ]] = stopped[[ paste0('cas_', x) ]]
	 sel= (stopped$cas_mode.int!=1 | stopped[[paste0('littlen_cas', x) ]]!=1)
	 stopped[[paste0('ped_cas_', x) ]][ sel] = NA
			
   #bysort accident_index: egen ped_cas_`x'=max(ped_cas_`x'_temp)
		vartemp = paste0('ped_cas_', x)
		stopped[[vartemp]][is.na(stopped[[vartemp]])] = 0 #allow grouping for NAs
		stopped.gr  = aggregate(stopped[[vartemp]], by = list(stopped$accident_index), 
		                        FUN = max)
		names(stopped.gr) = c('accident_index', paste0('ped_cas_', x,'_max'))
		stopped  = inner_join(stopped, stopped.gr, by= 'accident_index')
		
		stopped[[ paste0('ped_cas_', x) ]] = stopped[[ paste0('ped_cas_', x,'_max') ]] 
		
		#drop littlen_cas`x' ped_cas_`x'_temp
		stopped[[ paste0('ped_cas_', x,'_max')]]  = NULL
		stopped[[paste0('littlen_cas', x)]] = NULL
    
	  			}

#remove loop components & collect
rm(stopped.gr, by_stopped)
saveRDS(stopped, 'stopped.Rds')   #save for testing

##############################################################################
## DEFINE LARGEST AND SECOND LARGEST OTHER VEHICLES, TO BECOME STRIKE VEHICLE

#use stopped1 to merge later
stopped1 = subset(stopped, select = c(accident_index,veh_mode,veh_mode.int, 
                                      veh_reference, veh_male, veh_age, numped,
                                      ped_cas_male, ped_cas_age ))

# duplicates drop
stopped1 = stopped1[!duplicated(stopped1),]   
stopped1$veh_modei = -1 *stopped1$veh_mode.int
stopped1$veh_modei[stopped1$veh_modei == -99] = 99

set.seed(2011)
stopped1$random1 = runif(n = nrow(stopped1), min = 0, max = 1)

# by accident_index (veh_modei random1), sort: gen littlen=_n
by_stopped1 <- stopped1 %>% group_by(accident_index)   # groups by
stopped1 <- mutate(arrange(stopped1, accident_index, veh_modei, random1),
                   littlen=unlist(lapply(group_size(by_stopped1), FUN= seq_len)))


# keep accident_index veh_reference veh_mode veh_male veh_age littlen numped ped_cas_male ped_cas_age  
stopped1 = subset(x = stopped1, select = c(accident_index, veh_reference, veh_mode, 
                                           veh_male, veh_age, littlen, numped, 
                                           ped_cas_male, ped_cas_age)   )


# !! reshape wide veh_reference veh_mode veh_male veh_age, i(accident_index) j(littlen)
stopped1 = reshape(data = stopped1, v.names = c('veh_reference','veh_mode','veh_male','veh_age'),
                           timevar='littlen' , idvar = c('accident_index'),  direction = "wide")


for (x in c('reference','mode','male','age')) {
      names(stopped1)[names(stopped1)==paste0('veh_',x,'.1')] = paste0('veh_',x,'_firstlarge')
      names(stopped1)[names(stopped1)==paste0('veh_',x,'.2')] = paste0('veh_',x,'_secondlarge')
			}


stopped1$veh_mode_secondlarge[is.na(stopped1$veh_mode_secondlarge) & stopped1$numped!= 0 ]= 'pedestrian'  # 
stopped1$veh_mode_secondlarge[is.na(stopped1$veh_mode_secondlarge)] = 'NOV'

#replace values for age/male second large vehicle
sel = (stopped1$numped!=0 & stopped1$veh_mode_secondlarge == 'pedestrian')
stopped1$veh_male_secondlarge[sel] = stopped1$ped_cas_male[sel]
stopped1$veh_age_secondlarge[sel] = stopped1$ped_cas_age[sel]

stopped1 = subset(stopped1, select =c(accident_index, veh_reference_firstlarge, veh_reference_secondlarge,
                                    veh_mode_firstlarge, veh_mode_secondlarge, veh_male_firstlarge,
                                    veh_male_secondlarge, veh_age_firstlarge, veh_age_secondlarge)   )


write.csv(stopped1, file = './stats19_strikemode.csv')

#MERGE IN AND DEFINE STRIKE MODE - FOR NON-PEDESTRIANS, THIS IS LARGEST OTHER VEHICLE
stopped = inner_join(stopped, stopped1, by="accident_index")
rm(stopped1)

# prepare vars for loop
stopped= stopped[! is.na(stopped$cas_severity),  ]   # delete undefined severity
stopped$veh_mode_firstlarge = as.character(stopped$veh_mode_firstlarge)
stopped$veh_mode_secondlarge = as.character(stopped$veh_mode_secondlarge)

#output: 3 strike* vars w. integers categories
for (x in c('mode','male','age')) {

  stopped[[paste0('strike_', x) ]]= '0'   #creates the vars (char type imposed by factors treatment)

  sel= (stopped$cas_mode.int==1)
  stopped[[paste0('strike_', x) ]][sel] = stopped[[paste0('veh_', x) ]][sel]    # 1 if cas_mode.int==1, 0 otherwise
  
  
  sel= (stopped$cas_mode.int!=1) 
  stopped[[paste0('strike_', x) ]][sel] = stopped[[paste0('veh_', x,'_firstlarge') ]][sel]     
  
  sel= (stopped$veh_reference== stopped$veh_reference_firstlarge  & stopped$cas_mode.int!= 1)
  stopped[[paste0('strike_', x) ]][ sel ] = stopped[[paste0('veh_', x, '_secondlarge') ]][sel]
                      
                              }  

#recode as integers
stopped$strike_male = as.numeric(stopped$strike_male)
stopped$strike_age = as.numeric(stopped$strike_age)
stopped$strike_mode.int = recode(stopped$strike_mode, "pedestrian"='1', "cyclist"='2' , "motorcycle"='3',
                                 "car/taxi"='4', "light goods"='5', "bus"='6',
                                 "heavy goods"='7', "NOV"= '8',
                                 "other or unknown" = '99')

#IMPUTE AT RANDOM MISSING SEX OF A) CASUALTY AND B) STRIKER, 
# IN PROPORTION TO OBSERVED SEX RATIO OF STRIKER COLLISIONS FOR EACH MODE [not done for age]
set.seed(2012)
stopped$random3 = runif(n = nrow(stopped), min = 0, max = 1)
table(stopped$cas_male, useNA = "always")   # missing data 0.2% casualty sex

table(stopped$strike_male[stopped$strike_mode.int!=8] , useNA = "always" )  # missing data 6.5% striker sex


#to allow means to work operates on the .int variable
for (x in c('cas', 'strike')) {
  stopped[[paste0(x,'_mode_sexratio')]]= NA  #creates vars
			
  for (i in c(1:7,99) ) {
      sel= (stopped[[paste0(x, '_mode.int')]]==i)
			temp.mean = mean( stopped[[paste0(x, '_male') ]][sel], na.rm = T)  #the mean in the summary
			stopped[[ paste0(x, '_mode_sexratio')]][sel]=  temp.mean
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
stopped = arrange(stopped, accident_index, year, roadtype, cas_severity, cas_mode,
                  cas_male, cas_age, strike_mode, strike_male, strike_age)

#save a range of columns
# ncol1= which(names(stopped)=='accident_index')
# ncol2= which(names(stopped)=='strike_age')
# stopped = stopped [, c(ncol1:ncol2) ]

saveRDS(stopped, './1b_DataCreated/stats19_05-15_ready_v3.Rds')  # input for ITHIM conversion


# ***************************
#   * ANALYSIS
# ***************************


#define new vars
stopped$strike_modecat = recode(stopped$strike_mode.int, '3' =5, '5'=6, '6'=3,'8'=9, '99'=8 )
stopped$cas_mode_cat = recode(stopped$cas_mode.int, '3' =5, '5'=6, '6'=3,'8'=9, '99'=8 )


# recode  + add strike_mode_Cat labels var (this category does NOT MATCH previous ones)
stopped$modecatlab=recode(stopped$strike_modecat, "1"="walk","2"="cycle", "3"="bus",
                                              "4" = "car","5"= "mbike","6"= "van",
                                              "7"= "lorry","8"= "other", "9"="NOV" )  


#these are only crosstabs frequency tables

# CASUALTY PERSPECTIVE (only use pedestrian, cyclist, motorcycle, car/taxi)
# foreach i in 1 0 {
#   foreach j in 1 2 3 {
#     bysort cas_severity: tab cas_modecat strike_modecat if cas_male==`i' & roadtype==`j'
#   }
#   }

#     * STRIKER PERSPECTIVE
#     foreach i in 1 0 {
#     foreach j in 1 2 3 {
#     bysort cas_severity: tab strike_modecat cas_modecat if strike_male==`i' & roadtype==`j'
#     }
#     }
#     
