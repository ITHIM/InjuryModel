# 3-2-2017 MRC-Epid JHZ
# converted from Anna Goodman's Stata code

setwd("z_ITHIMfiles/Stats19/")

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
v2 <- c("accident_index","vehicle_reference", "vehicle_type", "sex_of_driver", "age_of_driver") 
v3 <- c("accident_index","vehicle_reference","casualty_reference", "casualty_class", "casualty_severity", "sex_of_casualty", "age_of_casualty")
av <- merge(Accidents0515[v1], Vehicles0515[v2], by="accident_index")
avc <- merge(av,Casualties0515[v3],by=c("accident_index","vehicle_reference"))

# Drop Wales and Scotland
stopped <- subset(avc,local_authority_.district.<=699)

setwd("../../")
