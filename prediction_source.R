#########################################################
## load injuries data ##

x <- readRDS('ssg.Rdata')
ssg <- x$ssg
covs <- names(ssg)[1:8]

#########################################################
## add SIN to injuries dataframe ##

modes <- levels(ssg$cas_mode)
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

cat('\nStriker safety-in-numbers exponents ("str_sin"):\n')
print(str_sin)
cat('\nCasualty safety-in-numbers exponents ("cas_sin"):\n')
print(cas_sin)

ssg.base <- ssg %>% 
  mutate(data_years = 11, # years over which the baseline data were collected
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
    
    fit <- glm(as.formula(formula), data=ssg.base, family=poisson)
    fitted.values <- fit$fitted.values
    AIC(fit)
    saveRDS(fit,file='catfit3.Rdata')
    saveRDS(fitted.values,file='catfit3values.Rdata')
  }
}

#########################################################
## get/create population and travel time data ##

#tmp <- as.matrix(read.table("../ITHIM/analytica_files/ages.txt")); 
N <- to.tensor(c(0.1242,0.1333,0.1488,0.16789,0.1129,0.1365,0.0465,0.0674,0.0251,0.0375),c(cas_male=2,cas_age_band=5))
N <- N[2:1,]
cat('\nPopulation make up ("N"):\n')
print(as.data.frame(N))
tmp <- as.matrix(read.table("times.txt")); 
TT <- to.tensor(as.vector(t(tmp)),c(cas_age_band=6,cas_mode=9,sce=6,cas_male=2))
TT <- TT[[cas_mode=c(1,2,5,4,6,3,7,8),sce=1,cas_male=2:1]]
cat('\nTravel times ("TT"):\n')
print(as.array(apply(TT,c(2:3),sum)))
# get the base-level total travel time
A.base0 <- TT[[cas_mode=1:8,cas_age_band=1:5]]*N
# divide total travel time amongst three road types
A.base <- to.tensor(1,dims=c(cas_male=2,cas_age_band=5,cas_mode=8,roadtype=3))
for(i in 1:length(unique(ssg.base$cas_mode)))
  for(j in 1:length(unique(ssg.base$cas_male)))
    for(k in 1:length(unique(ssg.base$cas_age_band))){
      roads <- vector()
      for(l in 1:length(unique(ssg.base$roadtype)))
        roads[l] <- sum(fitted.values[ssg.base$cas_mode==unique(ssg.base$cas_mode)[i]&
            ssg.base$strike_mode==unique(ssg.base$cas_mode)[i]&
            ssg.base$cas_male==unique(ssg.base$cas_male)[j]&
            ssg.base$cas_age_band==unique(ssg.base$cas_age_band)[k]&
            ssg.base$roadtype==unique(ssg.base$roadtype)[l]],na.rm=T)
      roads <- roads/sum(roads)
      A.base[[cas_mode=i,cas_male=j,cas_age_band=k]] <- as.numeric(A.base0[[cas_mode=i,cas_male=j]][k])*to.tensor(roads,dim=c(roadtype=3))
    }
cat('\nTotal travel ("A.base", female):\n')
print(as.array(apply(A.base,c(1,3,4),sum)[1,,]))
cat('\nTotal travel ("A.base", male):\n')
print(as.array(apply(A.base,c(1,3,4),sum)[2,,]))

#########################################################
## functions ##

get_scenario_template <- function(vars){
  dimnames <- list()
  dims <- vector()
  for(i in 1:length(vars)){
    dimnames[[i]] <- unique(ssg.base[[vars[i]]])
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
  dimind <- vector()
  for(i in 1:length(dimensions)){
    varind <- which(names(variables)==dimensions[i])
    if(!is.null(variables[[varind]])){
      ind <- which(names(ssg.base)==dimensions[i])
      dimind <- which(unique(ssg.base[[ind]])%in%variables[[varind]])
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
  for(i in 1:dim(operation_ind)[1]){
    for(k in 1:2){
      indices <- 1:dim(ssg.base)[1]
      for(j in 1:ndims)
        indices <- intersect(indices,which(ssg.base[[cas_strike_names[[k]][j]]]==unique(ssg.scen[[cas_strike_names[[k]][j]]])[operation_ind[i,j]]))
      ssg.scen[indices,ind[k]] <- as.numeric(scenario_table[t(operation_ind[i,])])
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
