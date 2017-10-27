#########################################################
## functions ##

get_ssg <- function(){
  # group data according to eight categorical variables
  load(file="Stats19.rda") # contains single object Stats19
  ss19$cas_age_band <- cut(ss19$cas_age,breaks=c(0,20,40,60,80,110),right=FALSE)
  ss19$strike_age_band <- cut(ss19$strike_age,breaks=c(0,16,25,60,80,110),right=FALSE)
  ss19<-tbl_df(ss19)
  ss19<-ss19[complete.cases(ss19),]
  ss19 <- ss19 %>% droplevels() %>%
    filter(cas_mode!='No other vehicle') %>%
    filter(strike_mode!='No other vehicle')
  ssg <-
    group_by(ss19,cas_mode,cas_severity,cas_age_band,cas_male,strike_age_band,strike_male,strike_mode,roadtype) %>% 
    summarise(count=n()) %>% 
    droplevels() %>% 
    as.data.frame() %>%    # remove "grouped" class, which breaks filling with zeroes
    complete(cas_mode,cas_severity,cas_age_band,cas_male,strike_age_band,strike_male,strike_mode,roadtype,fill=list(count=0))
  saveRDS(list(ssg=ssg),'ssg.Rdata')
}

get_SiN <- function(){
  # SiN from Analytica ITHIM
  cas_sin <- matrix(c(5,4,4,4,4,4,4,4,rep(5,times=7*8))/10,nrow=8,ncol=8,byrow=T) # victim by row
  str_sin <- matrix(c(rep(7,8),#ped
    7,5.5,rep(7,6),#cyc
    7,7,5.5,rep(7,5),#mot
    7,7,7,rep(5.5,5),#car
    7,7,7,rep(5.5,5),#lgv
    7,7,7,rep(5.5,5),#bus
    7,7,7,rep(5.5,5),#hgv
    7,7,7,rep(5.5,5))/10,#oth
    nrow=8,ncol=8,byrow=T) # victim by row
  saveRDS(list(cas_sin=cas_sin,str_sin=str_sin),file='SiN.Rdata')
}

get_fit <- function(){
  # get model interactions
  x <- readRDS('foreachModel.Rdata')
  all <- x$all
  formula <- 'count~offset(log(data_years))+offset(log(cas_reltime))+offset(log(strike_reltime))+
    offset(I(sin_cas * log(cas_reltime_group)))+offset(I(sin_strike * log(strike_reltime_group)))'
  indices <- c()
  for(tt in 1:70){
    formula <- paste(c(formula,all[[tt]]$asfactor),collapse='+')
    indices <- unique(c(indices,all[[tt]]$indices))
  }
  o <- c(1:length(covs))[-indices]
  if(length(o)>0) for(oo in 1:length(o))
    formula <- paste(c(formula,paste0('ssg[[',o[oo],']]')),collapse='+')
  # run glm
  fit <- glm(as.formula(formula), data=ssg.base, family=poisson)
  fitted.values <- fit$fitted.values
  AIC(fit)
  # save results
  saveRDS(fit,file='catfit3.Rdata')
  saveRDS(fitted.values,file='catfit3values.Rdata')
}

get_population_proportions <- function(){
  library(xlsx)
  # get population data
  url <- 'https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland/mid2015/ukandregionalpopulationestimates18382015.zip'
  temp <- tempfile()
  download.file(url,temp)
  files <- unzip(temp)
  popdat <- read.xlsx(files[2],sheetIndex=11) # choose England data
  popdat <- popdat[,c(3,dim(popdat)[2])] # choose most recent year (2015)
  total_pop <- as.numeric(as.character(popdat[2,2]))
  gen_pop <- list()
  gen_pop[[2]] <- popdat[98:188,] # male
  gen_pop[[1]] <- popdat[193:283,] # female
  N0 <- matrix(0,nrow=2,ncol=length(trip_ages))
  pop_ages <- c(trip_ages,110)
  pop_ages[length(pop_ages)] <- min(pop_ages[nall_ages],dim(gen_pop[[1]])[1]-1)
  # sum within relevant age groupings (those for which we have travel data) and divide by total population
  for(i in 1:(length(pop_ages)-1))
    for(j in 1:2)
      N0[j,i] <- sum(as.numeric(as.character(gen_pop[[j]][(pop_ages[i]+1):(pop_ages[i+1]+1),2])))/total_pop
  N0 <- as.data.frame(N0)
  names(N0) <- pop_ages[-(nall_ages)]
  saveRDS(list(N0=N0),file='population_proportions.Rdata')
}

get_travel_times <- function(){
  library(readODS)
  # get data on average trip time per mode
  url <- 'https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/632812/nts0311.ods'
  temp <- tempfile()
  download.file(url,temp)
  trip_times <- read.ods(temp,sheet=1) # average trip duration, minutes
  trip_times <- trip_times[-c(1:8,31:47),c(1,18)]
  trip_times <- as.numeric(trip_times[c(3,5,8,6,6,12,13,18),2])
  warning('Data for HGV trip times come from intercity buses. "Other" is the over-all average trip time.')
  # get data on number of trips made per mode by age and gender
  url <- 'https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/632847/nts0601.ods'
  temp <- tempfile()
  download.file(url,temp)
  trip_counts <- read.ods(temp,sheet=3) # trips per year
  trip_counts <- trip_counts[-c(1:8,55:70),-c(3)]
  trip_counts[-c(1:2),-c(1)] <- as.numeric(sapply(trip_counts[-c(1:2),-c(1)],function(x)gsub(',','',x)))
  trip_ages <- as.numeric(unlist(lapply(strsplit(as.character(trip_counts[1,3:10]),"[^0-9]+"),function(x)x[1])))
  trip_pop <- list()
  trip_pop[[1]] <- as.matrix(sapply(trip_counts[c(33,34,37,35,35,38,44,44),3:10],as.numeric),nrow=8,ncol=length(3:10))
  trip_pop[[2]] <- as.matrix(sapply(trip_counts[c(18,19,22,20,20,23,29,29),3:10],as.numeric),nrow=8,ncol=length(3:10))
  warning('Data for motorbike trip numbers come from "private transport", which include school buses.')
  trip_pop[[1]][4,1] <- 0.5
  trip_pop[[2]][4,1] <- 0.6
  warning('Data for car/taxi trip numbers come from "car/van", and don\'t include taxi.')
  mf_car_ratio <- sum(trip_pop[[1]][4,])/(sum(trip_pop[[2]][4,])+sum(trip_pop[[1]][4,]))
  trip_pop[[1]][5,] <- mf_car_ratio*car_hgv_ratio[2]*trip_pop[[1]][4,]
  trip_pop[[2]][5,] <- (1-mf_car_ratio)*car_hgv_ratio[2]*trip_pop[[1]][4,]
  warning('Data for LGV trip numbers come from car data scaled by LGV/car trip numbers and LGV-driver gender ratio.')
  trip_pop[[1]][7,] <- 0.01*car_hgv_ratio[1]*trip_pop[[1]][4,]
  trip_pop[[2]][7,] <- 0.99*car_hgv_ratio[1]*trip_pop[[1]][4,]
  warning('Data for HGV trip numbers come from car data scaled by HGV/car trip numbers and HGV-driver gender ratio.')
  trip_pop[[1]][8,] <- trip_pop[[1]][5,]
  trip_pop[[2]][8,] <- trip_pop[[1]][5,]
  warning('Data for "other or unknown" trip numbers same as LGV.')
  trip_pop <- lapply(trip_pop,function(x)x/365) # average number of trips per day
  trip_pop_T <- bind.tensor(A=as.tensor(trip_pop[[1]],dims=c(mode=8,age=8,gen=1)),'gen',B=as.tensor(trip_pop[[2]],dims=c(mode=8,age=8,gen=1)))
  TT1 <- trip_pop_T*as.tensor(as.numeric(trip_times),dims=c(mode=8))
  dimnames(TT1) <- list(modes,trip_ages,c(0,1))
  saveRDS(list(TT1=TT1),file='travel_times.Rdata')
}

get_4wd_data <- function(){
  library(readODS)
  # get data on miles travel on three road types by car, lgv and hgv
  url <- 'https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/644453/tra2503.ods'
  temp <- tempfile()
  download.file(url,temp)
  vehicle_road <- read.ods(temp,sheet=2)
  vehicle_road <- vehicle_road[-c(1:7,8,104:117),-c(3,10,15)]
  names(vehicle_road) <- c('year','quarter',
    'car/motorway','car/rural A','car/urban A','car/rural minor','car/urban minor','car/all',
    'hgv/motorwar','hgv/rural A','hgv/urban A','hgv/all',
    'lgv/motorway','lgv/rural A','lgv/urban A','lgv/rural minor','lgv/urban minor','lgv/all')
  for(i in 3:dim(vehicle_road)[2])
    vehicle_road[,i] <- as.numeric(vehicle_road[,i])
  vehicle_road[['hgv/other']] <- vehicle_road[,12] - rowSums(vehicle_road[,9:11])
  # get data on speed travelled by vehicles on different road types
  url <- 'https://www.gov.uk/government/uploads/system/uploads/attachment_data/file/623257/spe0111.ods'
  temp <- tempfile()
  download.file(url,temp)
  vehicle_road_speed <- read.ods(temp,sheet=1)
  vehicle_road_speed <-vehicle_road_speed[,-c(1,5:8,10:12)]
  names(vehicle_road_speed) <- c('limit','car','lgv','hgv23','hgv45')
  average_vehicle_road_speed <- vehicle_road_speed[c(25,45,61,76),]
  for(i in 2:dim(average_vehicle_road_speed)[2])
    average_vehicle_road_speed[,i] <- as.numeric(average_vehicle_road_speed[,i])
  average_vehicle_road_speed$hgv <- rowMeans(average_vehicle_road_speed[,c(4:5)])
  average_vehicle_road_speed <- average_vehicle_road_speed[,-c(4:5)]
  average_vehicle_road_speed <- average_vehicle_road_speed[,c(1,2,4,3)]
  average_vehicle_road_speed$limit <- c(70,60,30,20)
  # divide distance (miles) by speed (miles/hour) to get hours
  vehicle_road_hr <- vehicle_road
  vehicle_road_hr[,c(3,9,13)] <- matrix(unlist(apply(vehicle_road_hr[,c(3,9,13)],1,function(x)x/average_vehicle_road_speed[1,2:4])),ncol=3,byrow=T)
  vehicle_road_hr[,c(4,10,14)] <- matrix(unlist(apply(vehicle_road_hr[,c(4,10,14)],1,function(x)x/average_vehicle_road_speed[2,2:4])),ncol=3,byrow=T)
  vehicle_road_hr[,c(5,11,15)] <- matrix(unlist(apply(vehicle_road_hr[,c(5,11,15)],1,function(x)x/average_vehicle_road_speed[3,2:4])),ncol=3,byrow=T)
  vehicle_road_hr[,c(6,7,19,16,17)] <- matrix(unlist(apply(vehicle_road_hr[,c(6,7,19,16,17)],1,function(x)x/average_vehicle_road_speed[3,c(2,2,3,4,4)])),ncol=5,byrow=T)
  vehicle_road_hr[,8] <- rowSums(vehicle_road_hr[,c(3:7)])
  vehicle_road_hr[,12] <- rowSums(vehicle_road_hr[,c(9:11,19)])
  vehicle_road_hr[,18] <- rowSums(vehicle_road_hr[,c(13:17)])
  # calculate proportion of time spent by each vehicle type on each road type
  car_roads2 <- colMeans(cbind(vehicle_road_hr[46:93,3],rowSums(vehicle_road_hr[46:93,4:5]),rowSums(vehicle_road_hr[46:93,6:7]))/vehicle_road_hr[46:93,8])
  hgv_roads2 <- colMeans(cbind(vehicle_road_hr[46:93,9],rowSums(vehicle_road_hr[46:93,10:11]),vehicle_road_hr[46:93,19])/vehicle_road_hr[46:93,12])
  lgv_roads2 <- colMeans(cbind(vehicle_road_hr[46:93,13],rowSums(vehicle_road_hr[46:93,14:15]),rowSums(vehicle_road_hr[46:93,16:17]))/vehicle_road_hr[46:93,18])
  car_hgv_ratio <- c(vehicle_road_hr[93,12],vehicle_road_hr[93,18])/vehicle_road_hr[93,8]
}

get_scenario_template <- function(vars){
  dimnames <- list()
  dims <- vector()
  for(i in 1:length(vars)){
    if(vars[i]=='cas_age_band'){
      dimnames[[i]] <- unlist(lapply(1:(nall_ages-1),function(x)paste0('[',all_ages[x],',',all_ages[x+1],')')))
    }else{
      dimnames[[i]] <- unique(ssg.base[[vars[i]]])
    }
    dims[i] <- length(dimnames[[i]])
  }
  scenario <- to.tensor(1,dims=dims,ndimnames=dimnames)
  names(scenario) <- vars
  return(scenario)
}

edit_scenario_template <- function(scenario_table,cas_mode=NULL,cas_age_band=NULL,cas_male=NULL,roadtype=NULL,factor=1){
  variables <- list(cas_mode=cas_mode,cas_age_band=cas_age_band,cas_male=cas_male,roadtype=roadtype)
  dimensions <- names(scenario_table)
  arrind <- array(T,dim=dim(scenario_table))
  allind <- do.call(expand.grid,lapply(dim(scenario_table),seq))
  for(i in 1:length(dimensions)){
    varind <- which(names(variables)==dimensions[i])
    if(!is.null(variables[[varind]])){
      dimind <- which(dimnames(scenario_table)[[i]]%in%variables[[varind]])
      for(j in 1:dim(allind)[1])
        if(!allind[j,i]%in%dimind)
          arrind[t(unlist(allind[j,]))] <- F
    }
  }
  scenario_table[arrind] <- factor*scenario_table[arrind]
  return(scenario_table)
}

create_scenario <- function(scenario_table=NULL,ssg.base=ssg.base,A.base=A.base){
  # define basic variables
  ssg.scen <- ssg.base
  dims <- dim(scenario_table)
  ndims <- length(dims)
  cas_names <- c('cas_male','cas_age_band','cas_mode','roadtype')
  strike_names <- c('strike_male','strike_age_band','strike_mode','roadtype')
  # check input data
  if(sum(c('strike_male','strike_age_band','strike_mode')%in%names(scenario_table))>0){
    warning('Striker and casualty relative times are identical.\nUse only casualty variables in the scenario definition.')
    return(0)
  }
  if(sum(!names(scenario_table)%in%cas_names)>0){
    warning('Use (some of) the following variables to construct the scenario: \ncas_male, cas_age_band, cas_mode, roadtype')
    return(0)
  }
  # get reltime_group
  modes <- levels(ssg.scen$cas_mode)
  roads <- levels(ssg.scen$roadtype)
  ind2 <- which(names(ssg.scen)%in%c('cas_reltime_group','strike_reltime_group'))
  A.scen <- scenario_table*A.base
  A.scen.hat <- apply(A.scen,which(names(A.scen)%in%c('cas_mode','roadtype')),sum)/apply(A.base,which(names(A.base)%in%c('cas_mode','roadtype')),sum)
  #cat('\nSafety-in-numbers bases:\n')
  #print(A.scen.hat)
  if(dim(A.scen.hat)[1]<dim(A.scen.hat)[2]) A.scen.hat <- t(A.scen.hat)
  for(i in 1:length(modes))
    for(j in 1:length(roads)){
      ssg.scen[ssg.scen$cas_mode==modes[i]&ssg.scen$roadtype==roads[j],ind2[1]] <- A.scen.hat[i,j]
      ssg.scen[ssg.scen$strike_mode==modes[i]&ssg.scen$roadtype==roads[j],ind2[2]] <- A.scen.hat[i,j]
    }
  # get rel_times
  operation_ind <- as.matrix(do.call(expand.grid,lapply(dim(scenario_table),seq)))
  ind <- which(names(ssg.scen)%in%c('cas_reltime','strike_reltime'))
  cas_strike_names <- list(names(scenario_table),strike_names[match(names(scenario_table),cas_names)])
  cas_strike_ages <- list(cas_ages,strike_ages)
  for(i in 1:dim(operation_ind)[1]){
    for(k in 1:2){
      indices <- 1:dim(ssg.base)[1]
      fraction <- 1
      for(j in 1:ndims){
        if(cas_strike_names[[k]][j]%in%c('cas_age_band','strike_age_band')){
          input_ages <- all_ages[operation_ind[i,j]:(operation_ind[i,j]+1)]
          age_index <- which(cas_strike_ages[[k]]<=input_ages[1])
          age_index <- age_index[length(age_index)]
          indices <- intersect(indices,which(ssg.base[[cas_strike_names[[k]][j]]]==unique(ssg.scen[[cas_strike_names[[k]][j]]])[age_index]))
          fraction <- (input_ages[2]-input_ages[1])/(cas_strike_ages[[k]][age_index+1]-cas_strike_ages[[k]][age_index])
          #print(c(input_ages,cas_strike_ages[[k]][age_index:(age_index+1)],fraction))
        }else{
          indices <- intersect(indices,which(ssg.base[[cas_strike_names[[k]][j]]]==unique(ssg.scen[[cas_strike_names[[k]][j]]])[operation_ind[i,j]]))
        }
      }
      ssg.scen[indices,ind[k]] <- (1-fraction)*ssg.scen[indices,ind[k]]+as.numeric(scenario_table[t(operation_ind[i,])])*fraction
      #print(c(i,k,fraction,as.numeric(scenario_table[t(operation_ind[i,])]),(1-fraction)*1+as.numeric(scenario_table[t(operation_ind[i,])])*fraction))
    }
  }
  ssg.scen$cas_reltime <- ssg.scen$cas_reltime/ssg.scen$cas_reltime_group
  ssg.scen$strike_reltime <- ssg.scen$strike_reltime/ssg.scen$strike_reltime_group
  return(ssg.scen)
}

scenario_predictions <- function(fit=NULL,fitted.values=fitted.values,newdata,get.se=FALSE){
  if(get.se==TRUE){
    preds <- predict(fit, newdata=newdata, se.fit=TRUE, type="response")
    return(cbind(newdata,preds$fit,preds$se.fit))
  }else{
    preds <- fitted.values*newdata$cas_reltime*newdata$cas_reltime_group^newdata$sin_cas*
      newdata$strike_reltime*newdata$strike_reltime_group^newdata$sin_strike
    return(cbind(newdata,preds))
  }
}
