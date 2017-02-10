# 10-2-2017 MRC-Epid JHZ
# conversion of Anna Goodman's Stata code adding direct Internet access

# function to append files as with dplyr::bind_rows()
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

# function To get and merge 2005-2014 with 2015 datasets 

get.data <- function(is.local=TRUE, local.dir=".")
{
  if(!is.local) {
  # url0514 contains file0514 from 2005-2014
  url0514 <- "http://data.dft.gov.uk.s3.amazonaws.com/road-accidents-safety-data/Stats19_Data_2005-2014.zip"
  # individual files
  file0514 <- c("Vehicles0514.csv","Casualties0514.csv","Accidents0514.csv")
  ## version for 2014 only
  ## file0514 <- paste("DfTRoadSafety_",c("Vehicles_2014.csv","Casualties_2014.csv","Accidents_2014.csv"),sep="")

  temp <- tempfile()
  download.file(url0514,temp)
  ## download.file("http://data.dft.gov.uk/road-accidents-safety-data/DfTRoadSafety_Accidents_2014.zip",temp)
  a0514 <<- read.csv(unz(temp, file0514[3]))
  ## download.file("http://data.dft.gov.uk/road-accidents-safety-data/DfTRoadSafety_Casualties_2014.zip",temp)
  c0514 <<- read.csv(unz(temp, file0514[2]))
  ## download.file("http://data.dft.gov.uk/road-accidents-safety-data/DfTRoadSafety_Vehicles_2014.zip",temp)
  v0514 <<- read.csv(unz(temp, file0514[1]))
  ### under Windows but not Linux
  names(a0514)[names(a0514)=="ï..Accident_Index"] <- "Accident_Index"
  names(c0514)[names(c0514)=="ï..Accident_Index"] <- "Accident_Index"
  names(v0514)[names(v0514)=="ï..Accident_Index"] <- "Accident_Index"

  # url2015 contains file2015
  file2015 <- c("Vehicles_2015.csv","Casualties_2015.csv","Accidents_2015.csv")
  url2015 <- "http://data.dft.gov.uk/road-accidents-safety-data/RoadSafetyData_2015.zip"
  download.file(url2015,temp)
  v2015 <<- read.csv(unz(temp, file2015[1]))
  c2015 <<- read.csv(unz(temp, file2015[2]))
  a2015 <<- read.csv(unz(temp, file2015[3]))
  assign("Accidents0515",fastmerge(a0514,a2015), envir=.GlobalEnv)
  assign("Casualties0515",fastmerge(c0514,c2015), envir=.GlobalEnv)
  assign("Vehicles0515",fastmerge(v0514,v2015), envir=.GlobalEnv)
  unlink(temp)
  } 
  else {
  # data are downloaded locally
  wd <- getwd()
  setwd(local.dir)

  for (x in c("Accidents", "Casualties", "Vehicles"))
  {
    cat(x, "\n")
    in2015 <- read.csv(paste("1a_DataOriginal/",x,"_2015.csv",sep=""))
    in0514 <- read.csv(paste("1a_DataOriginal/",x,"0514.csv",sep=""))
    ### see above
    names(in0514)[names(in0514)=="ï..Accident_Index"] <- "Accident_Index"
    assign(paste(x,'0515',sep=""), fastmerge(in0514,in2015), envir=.GlobalEnv)
  }
  setwd(wd)
  }
}

# The URL https://data.gov.uk/dataset/road-accidents-safety-data contains the 
# 2005-2014 and 2015 data, which can be directly accessed as follows,

get.data(is.local=FALSE)

# rm(v0514,c0514,a0514,v2015,a2015,c2015)
# For codebook see https://discover.ukdataservice.ac.uk/catalogue?sn=7752

# Optionally, pre-downloaded data can also be processed, assumming in 
# 'z_ITHIMFILES/Stats19/1a_DataOriginal' while the output is an empty folder
# 'z_ITHIMFILES/Stats19/1b_DataCreated' containing '0-temp' subdirectory

# get.data(local.dir="z_ITHIMfiles/Stats19")

names(Accidents0515) <- tolower(names(Accidents0515))
names(Casualties0515) <- tolower(names(Casualties0515))
names(Vehicles0515) <- tolower(names(Vehicles0515))

# Merge three datasets keeping required variables
v1 <- c("accident_index", "local_authority_.district.", "x1st_road_class", "date", "number_of_vehicles")
v2 <- c("accident_index","vehicle_reference", "vehicle_type", "sex_of_driver", "age_of_driver") 
v3 <- c("accident_index","vehicle_reference","casualty_reference", "casualty_class", "casualty_severity", "sex_of_casualty", "age_of_casualty")
av <- merge(Accidents0515[v1], Vehicles0515[v2], by="accident_index",all=TRUE)
avc <- merge(av,Casualties0515[v3],by=c("accident_index","vehicle_reference"),all=TRUE)

# Drop Wales and Scotland
stopped <- subset(avc,local_authority_.district.<=699)
