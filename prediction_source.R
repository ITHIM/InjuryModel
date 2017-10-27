
#########################################################
## load functions ##

source('prediction_functions.R')

#########################################################
## load injuries data ##

if(file.exists('ssg.Rdata')){
  x <- readRDS('ssg.Rdata')
  ssg <- x$ssg
}else{
  get_ssg()
}

#########################################################
## summarise injuries data ##

ncovs <- dim(ssg)[2]-1
covs <- names(ssg)[1:ncovs]
cat('Covariate data:\n')
print(covs)
cat('Road types:\n')
roadtype <- levels(ssg$roadtype)
nroadtypes <- length(roadtype)
print(roadtype)
cat('Modes:\n')
modes <- levels(ssg$cas_mode)
nmodes <- length(modes)
print(modes)
cat('Casualty ages:\n')
cas_age_band <- levels(ssg$cas_age_band)
print(cas_age_band)
cat('Striker ages:\n')
strike_age_band <- levels(ssg$strike_age_band)
print(strike_age_band)

cas_ages <- unique(na.omit(as.numeric(unlist(strsplit(cas_age_band,"[^0-9]+"))))) # cas_ages: casualty age boundaries in ssg dataset and fitted model
strike_ages <- unique(na.omit(as.numeric(unlist(strsplit(strike_age_band,"[^0-9]+"))))) # strike_ages: striker age boundaries in ssg dataset and fitted model
all_ages <- sort(unique(c(cas_ages,strike_ages))) ## all_ages: union of casualty and striker age boundaries in ssg dataset and fitted model
nall_ages <- length(all_ages)

#########################################################
## add SIN to injuries dataframe ##

if(file.exists('SiN.Rdata')){
  x <- readRDS('SiN.Rdata')
  str_sin <- x$str_sin
  cas_sin <- x$cas_sin
}else{
  get_SiN()
}

cat('\nStriker safety-in-numbers exponents ("str_sin"):\n')
print(str_sin)
cat('\nCasualty safety-in-numbers exponents ("cas_sin"):\n')
print(cas_sin)

years <- 11

ssg.base <- ssg %>% 
  mutate(data_years = years, # years over which the baseline data were collected
         ## Relative exposures/risks are 1 under the baseline scenario
         cas_reltime = 1,              # cv in the formula above
         strike_reltime = 1,           # cs 
         cas_reltime_group = 1,        # av
         strike_reltime_group = 1,     # as 
         ## Power coefficients.
         sin_cas = apply(cbind(ssg$cas_mode,ssg$strike_mode),1,function(x)cas_sin[x[1],x[2]]),  # bv
         sin_strike = apply(cbind(ssg$cas_mode,ssg$strike_mode),1,function(x)str_sin[x[1],x[2]]) # bs
  )

#########################################################
## load regression model ##

if(file.exists('catfit3.Rdata')){
  fit <- readRDS('catfit3.Rdata')
  fitted.values <- fit$fitted.values
}else{
  if(file.exists('catfit3values.Rdata')){
    fitted.values <- readRDS('catfit3values.Rdata')
  }else{
    get_fit()
  }
}

################################################
## get travel time data ##

if(file.exists('travel_times.Rdata')){
  x <- readRDS('travel_times.Rdata')
  TT1 <- x$TT1
}else{
  get_travel_times()
}

cat('\nTravel times ("TT1"):\n')
print(as.array(apply(TT1,c(1,3),sum)))
trip_ages <- as.numeric(dimnames(TT1)[[2]])
travel_ages <- c(trip_ages,110) # travel_ages: as defined by travel data, currently as input into analytica
## convert travel times to cover all age categories
TT.impute <- to.tensor(0,dims=c(cas_age_band=nall_ages-1,cas_mode=8,cas_male=2))
for(i in 1:(nall_ages-1)){
  ages <- c(all_ages[i:(i+1)])
  ind1 <- which(travel_ages <= ages[1])
  ind2 <- which(travel_ages >= ages[2])
  for(k in 1:2)
    for(l in 1:nmodes)
      for(j in ind1[length(ind1)]:(ind2[1]-1))
        TT.impute[i,l,k] <- TT.impute[i,l,k]+
      TT1[l,j,k]/(ages[2]-ages[1])*(min(ages[2],travel_ages[j+1])-max(ages[1],travel_ages[j]))
}
TT <- TT.impute

#########################################################
## get population data ##

if(file.exists('population_proportions.Rdata')){
  x <- readRDS('population_proportions.Rdata')
  N0 <- x$N0
}else{
  get_population_proportions()
}

cat('\nPopulation make up ("N0"):\n')
print(N0)
data_ages <- c(as.numeric(names(N0)),110) # age boundaries in dataset (currently defined to be all_ages, from ssg)
cat('\nAge boundaries for input travel data ("data_ages"):\n')
print(data_ages)

## impute travel times for ages based on input data
N.impute <- matrix(0,nrow=2,ncol=nall_ages-1)
for(i in 1:(nall_ages-1)){
  ages <- c(all_ages[i:(i+1)])
  ind1 <- which(data_ages <= ages[1])
  ind2 <- which(data_ages >= ages[2])
  for(k in 1:2)
    for(j in ind1[length(ind1)]:(ind2[1]-1))
      N.impute[k,i] <- N.impute[k,i]+
    N0[k,j]/(data_ages[j+1]-data_ages[j])*(min(ages[2],data_ages[j+1])-max(ages[1],data_ages[j]))
}
N <- N.impute/sum(N.impute)
cat('\nPopulation ("N"):\n')
print(N)
cat('with adjusted age groups ("all_ages"):\n')
print(all_ages)

##########################################################
# get the base-level total travel time
A.base0 <- TT*as.tensor(N,dims=c(cas_male=2,cas_age_band=(nall_ages-1)))
# divide total travel time amongst three road types
A.base <- to.tensor(1,dims=c(cas_male=2,cas_age_band=(nall_ages-1),cas_mode=nmodes,roadtype=3),dimnames=list(c(0,1),all_ages,modes,roadtype))
dimnames(A.base) <- list(cas_male=c(0,1),cas_age_band=all_ages[-nall_ages],cas_mode=modes,roadtype=roadtype)
for(i in 1:length(unique(ssg.base$cas_mode)))
  for(j in 1:length(unique(ssg.base$cas_male)))
    for(k in 1:(nall_ages-1)){
      ages <- c(all_ages[k:(k+1)])
      ind1 <- which(cas_ages <= ages[1])
      ind2 <- which(cas_ages >= ages[2])
      roads <- rep(0,length=3)
      for(m in ind1[length(ind1)]:(ind2[1]-1))
        for(l in 1:length(unique(ssg.base$roadtype)))
          roads[l] <- roads[l] +
            sum(fitted.values[ssg.base$cas_mode==unique(ssg.base$cas_mode)[i]&
              #ssg.base$strike_mode==unique(ssg.base$cas_mode)[i]&
              ssg.base$cas_male==unique(ssg.base$cas_male)[j]&
              ssg.base$cas_age_band==unique(ssg.base$cas_age_band)[m]&
              ssg.base$roadtype==unique(ssg.base$roadtype)[l]],na.rm=T)/
            (ages[2]-ages[1])*(min(ages[2],cas_ages[m+1])-max(ages[1],cas_ages[m]))
      roads <- roads/sum(roads)
      A.base[[cas_mode=i,cas_male=j,cas_age_band=k]] <- as.numeric(A.base0[[cas_mode=i,cas_male=j]][k])*to.tensor(roads,dim=c(roadtype=3))
    }
cat('\nTotal travel ("A.base", female):\n')
print(as.array(apply(A.base,c(1,3,4),sum)[1,,]))
cat('\nTotal travel ("A.base", male):\n')
print(as.array(apply(A.base,c(1,3,4),sum)[2,,]))

