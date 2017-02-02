# 2-2-2017 MRC-Epid JHZ, AU
# converted from Anna Goodman's Stata code

# setwd("C:\\Users\\Jing Hua Zhao\\modeling\\InjuryModel\\z_ITHIMfiles\\Stats19")

# DOWNLOAD 2005-2014 AND 2015 DATA FROM https://data.gov.uk/dataset/road-accidents-safety-data. SAVE THESE IN 'Stats19\1a_DataOriginal' FOLDER
# DOWNLOAD CODEBOOK FROM https://discover.ukdataservice.ac.uk/catalogue?sn=7752
# MAKE AN EMPTY FOLDER '1b_DataCreated', IN WHICH '0-temp'

# ***************************
# * PREPARE DATASETS
# ***************************	
# ** MERGE 2005-2014 AND 2015 DATASETS 
# http://stackoverflow.com/questions/8169323/r-concatenate-two-dataframes
fastmerge <- function(d1, d2) {
  d1.names <- names(d1)
  d2.names <- names(d2)
  # columns in d1 but not in d2
  d2.add <- setdiff(d1.names, d2.names)
  # columns in d2 but not in d1
  d1.add <- setdiff(d2.names, d1.names)
  # add blank columns to d2
  if(length(d2.add) > 0) {
    for(i in 1:length(d2.add)) {
      d2[d2.add[i]] <- NA
    }
  }
  # add blank columns to d1
  if(length(d1.add) > 0) {
    for(i in 1:length(d1.add)) {
      d1[d1.add[i]] <- NA
    }
  }
  return(rbind(d1, d2))
}
for (x in c("Accidents", "Casualties", "Vehicles"))
{
   cat(x, "\n")
   in2015 <- read.csv(paste("1a_DataOriginal/",x,"_2015.csv",sep=""))
   in2014 <- read.csv(paste("1a_DataOriginal/",x,"0514.csv",sep=""))
   names(in2014)[names(in2014)=="ï..Accident_Index"] <- "Accident_Index"
   assign(paste(x,'0515',sep=""), fastmerge(in2014,in2015))
   rm(in2014,in2015)
}
names(Accidents0515) <- tolower(names(Accidents0515))
names(Casualties0515) <- tolower(names(Casualties0515))
names(Vehicles0515) <- tolower(names(Vehicles0515))

# MERGE 3 DATASETS, KEEPING REQUIRED VARIABLES
v1 <- c("accident_index", "local_authority_.district.", "x1st_road_class", "date", "number_of_vehicles")
v2 <- c("accident_index","vehicle_reference", "vehicle_type", "sex_of_driver") 
v3 <- c("accident_index","vehicle_reference","casualty_reference", "casualty_class", "casualty_severity", "sex_of_casualty")
av <- merge(Accidents0515[v1], Vehicles0515[v2], by="accident_index")
avc <- merge(av,Casualties0515[v3],by=c("accident_index","vehicle_reference"))
# Drop Wales and Scotland
stopped <- subset(avc,local_authority_.district.<=699)

library(stringr)
library(readstata13)
library(dplyr)

# stopped = readstata13::read.dta13('stopped.dta')
stopped = stopped[, c(1:12)]   # clean odd columns errors

stopped = dplyr::rename(stopped,  cas_severity = casualty_severity )
severitylab=data.frame(cas_severity=c(1,2,3), severitylab=c("Fatal","Serious","Slight"))

# adds severity lab column (WATCH OUT NA LINES)
stopped=inner_join(stopped, severitylab, by="cas_severity")
stopped$severitylab = as.character(stopped$severitylab) # to char type

# DATE
td = str_split(string = stopped$date,pattern = "/",n = 3, simplify = TRUE)
stopped$year = td[,3]
rm(td)

# recode ROAD CLASS
stopped$st_road_class <- recode(stopped$x1st_road_class,'1'=1, '2' = 1, '3'=2, '4'=3,'5'=3, '6'=3) #1 stays unchanged

#add column roadtypelab
roadtypelab=data.frame(st_road_class=c(1,2,3), roadtypelab=c("Motorway/A(M)", "A","B, C, Unclassified"))
stopped = inner_join(stopped, roadtypelab, by="st_road_class")

stopped$roadtypelab = as.character(stopped$roadtypelab) #to chr type

# MODE AND SEX OF VEHICLE AND CASUALTY
stopped$veh_mode = recode(stopped$vehicle_type,'-1'=99, '1'=2, '2'=3,'3'=3, '4'=3, '5'=3, '8'=4, '9'=4,
 '10'=6, '11'=6, '16'=99, '17'=99,'18'=99, '19'=5, '20'=7, '21'=7, '22'=99,
 '23'=3, '90'=99, '97'=3, '98'=7 )


stopped$cas_mode = stopped$veh_mode
stopped$cas_mode[ stopped$casualty_class==3] = 1
stopped$cas_mode[ is.na(stopped$casualty_class)] = NA

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
stopped$pedflag[stopped$cas_mode==1] = 1 # CHECK : 1 if cas_mode=1, 0, otherwise
stopped$pedflag[stopped$cas_mode!=1] = 0


# check
stopped= arrange(stopped, accident_index)
stopped.gr = dplyr::group_by(.data = stopped,pedflag)

stopped= inner_join(stopped, stopped.gr, by="accident_index")

#create male flag
stopped$malepedflag=NULL
stopped$malepedflag[stopped$cas_mode==1 & stopped$cas_male==1] = 1
stopped.gr = base::aggregate(stopped$malepedflag, data=stopped , FUN=sum)
names(stopped.gr)=c('a', 'b') ## !!

#create female flag
stopped$femalepedflag=NULL
stopped$femalepedflag[stopped$cas_mode==1 & stopped$cas_male==0] = 1
stopped.gr = base::aggregate(stopped$femalepedflag, data=stopped , FUN=sum)

names(stopped.gr)=c('a', 'numfemaleped')

##inner_join()

drop=c('pedflag','malepedflag', 'femalepedflag')
stopped = stopped[, ! (names(stopped) %in% drop) ]

# ## save as .intermediate .CSV file

z <- function()
{
# PREPARE VARIABLES

# SEVERITY
rename casualty_severity cas_severity
label define severitylab 1 "Fatal" 2 "Serious" 3 "Slight"
label values cas_severity severitylab

# DATE
split date,p("/")
destring date3, gen(year)

# ROAD CLASS
recode st_road_class 1/2=1 3=2 4/6=3 , gen(roadtype)
label def roadtypelab 1 "Motorway/A(M)" 2 "A" 3 "B, C, Unclassified"
label val roadtype roadtypelab

# MODE AND SEX OF VEHICLE AND CASUALTY
recode vehicle_type -1=99 1=2 2/5=3 8/9=4 10/11=6 16/18=99 19=5 20/21=7 22=99 23=3 90=99 97=3 98=7 , gen(veh_mode)
gen cas_mode=veh_mode
replace cas_mode=1 if casualty_class==3
replace cas_mode=. if cas_severity==.
label define modelab 1 "pedestrian" 2 "cyclist" 3 "motorcycle" 4 "car/taxi" 5 "light goods" 6 "bus" 7 "heavy goods" 8 "No other vehicle" 99 "other or unknown", modify
label values veh_mode modelab
label values cas_mode modelab

recode sex_of_casualty -1=. 2=0, gen(cas_male)
recode sex_of_driver -1=. 2=0 3=., gen(veh_male)		

# NUMBER OF PEDESTRIANS, OF EACH SEX, IN ACCIDENT
gen pedflag=(cas_mode==1)
bysort accident_index: egen numped=sum(pedflag)			
gen malepedflag=(cas_mode==1 & cas_male==1)
bysort accident_index: egen nummaleped=sum(malepedflag)
gen femalepedflag=(cas_mode==1 & cas_male==0)
bysort accident_index: egen numfemaleped=sum(femalepedflag)
drop pedflag malepedflag femalepedflag	

# SRIKE MODE
// preserve
v <- c("accident_index", "veh_mode", "vehicle_reference", "veh_male", "numped", "nummaleped", "numfemaleped")
data <- "all current data"
data <- unique(data[v])
veh_modei <- -1*veh_mode
veh_modei <- replace(veh_modei, veh_modei==-99, 99)
set.seed(2011)
random1 <- runif(dim(data)[1])
for(x in order(accident_index veh_modei random1)) littlen <- _n
v <- c("accident_index", "veh_mode", "veh_male", "littlen", "numped", "nummaleped", "numfemaleped")
reshape wide veh_mode veh_male, i(accident_index) j(littlen)
foreach x in mode male {
rename veh_`x'1 veh_`x'_firstlarge
rename veh_`x'2 veh_`x'_secondlarge
}
random2 <- runif(dim(data)[1])
recode veh_mode_secondlarge .=1 if numped!=0
recode veh_male_secondlarge .=0 if numped!=0 & (numfemaleped>nummaleped | (numfemaleped==nummaleped & random2<0.5))
recode veh_male_secondlarge .=1 if numped!=0 & (numfemaleped<nummaleped | (numfemaleped==nummaleped & random2>=0.5))
recode veh_mode_secondlarge .=8
keep accident_index veh_mode_firstlarge veh_mode_secondlarge veh_male_firstlarge veh_male_secondlarge
saveold "1b_DataCreated\0-temp\stats19_strikemode.dta", replace
restore

merge m:1 accident_index using "1b_DataCreated\0-temp\stats19_strikemode.dta", nogen
keep if cas_severity!=. // drop those not in casualty dataset
foreach x in mode male {
gen strike_`x'=veh_`x'_firstlarge
replace strike_`x'=veh_`x'_secondlarge if veh_mode_firstlarge==cas_mode & (veh_male_firstlarge==cas_male | veh_male_firstlarge==. | cas_male==.) // use second largest if signs that largest might be the casualty themselves
replace strike_`x'=. if cas_`x'==.
}
label values strike_mode modelab

# IMPUTE AT RANDOM MISSING A) CASUALTY AND B) STRIKER SEX, IN PROPORTION TO OBSERVED SEX RATIO OF STRIKER COLLISIONS FOR EACH MODE
set.seed(2012)
gen random3=uniform()		
tab cas_male , miss 
# missing data 0.2% casualty sex
tab strike_male if strike_mode!=8, miss 
# missing data 6.5% striker sex
foreach x in cas strike {
gen `x'_mode_sexratio=.
foreach i in 1 2 3 4 5 6 7 99 {
sum `x'_male if `x'_mode==`i'
replace `x'_mode_sexratio=r(mean) if `x'_mode==`i'
}
}
recode cas_male .=1 
if(random3<=cas_mode_sexratio & cas_severity!=.)
recode cas_male .=0 if random3>cas_mode_sexratio & cas_severity!=.
recode strike_male .=1 if random3<=strike_mode_sexratio & strike_mode!=8
recode strike_male .=0 if random3>strike_mode_sexratio & strike_mode!=8			

#SAVE
order <- c("accident_index", "year", "roadtype", "cas_severity", "cas_mode", "cas_male", "strike_mode", "strike_male")
keep accident_index-strike_male
foreach var of varlist accident_index-strike_male {
  label variable `var'
}
// compress
save(data, file="table.rda")

#
#***************************
# ANALYSIS
#***************************		
use "table.dta", clear
recode strike_mode 3=5 5=6 6=3 7=7 8=9 99=8, gen(strike_modecat)
recode cas_mode 3=5 5=6 6=3 7=7 8=9 99=8, gen(cas_modecat)
label define modecatlab 1 "walk" 2 "cycle" 3 "bus" 4 "car" 5 "mbike" 6 "van" 7 "lorry" 8 "other" 9 "no other vehicle" , modify
label values strike_modecat modecatlab
label values cas_modecat modecatlab
# CASUALTY PERSPECTIVE (only use pedestrian, cyclist, motorcycle, car/taxi)
foreach i in 1 0 {
  foreach j in 1 2 3 {
  bysort cas_severity: tab cas_modecat strike_modecat if cas_male==`i' & roadtype==`j'
  }
}

# STRIKER PERSPECTIVE
foreach i in 1 0 {
  foreach j in 1 2 3 {
  bysort cas_severity: tab strike_modecat cas_modecat if strike_male==`i' & roadtype==`j'
  }
}
}
