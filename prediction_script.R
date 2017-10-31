## Demo of how to do alternative scenarios in Poisson model 

## The idea is to implement the log-linear model currently used in the Analytica model for safety in numbers

## rate(i) = base_rate(i) * av(i)^bv * cv(i) * as(i)^bs * cs(i)

## where av,bv,cv,as,bs,cs are numbers governing the relative risks in alternative scenarios for victims and strikers, related to changes in distance/time at risk.

setwd("~/OneDrive/injury_predictions"); 
library(dplyr)
library(tensorA)

# load data and functions
source('prediction_source.R')

# create empty variables
scenario <- list()
ssg.scen <- list()

##########################################
## PART 1: Create scenarios ##

## scenario 1: increase cyclists and pedestrians on A, B and C roads; decrease all cars
# select variables to adjust.
# choose between: 'cas_age_band','cas_male','cas_mode','roadtype'
vars <- c('cas_mode','roadtype')
# generate scenario template.
scenario[[1]] <- get_scenario_template(vars)
# adjust template using 'edit_scenario_template'. 
# Supply: 
  # the scenario_table just generated to work on; 
  # the categories within the covariates to be adjusted; 
  # and the factor by which the value(s) should be scaled
scenario[[1]] <- edit_scenario_template(scenario_table=scenario[[1]],cas_mode=c('cyclist','pedestrian'),roadtype=c('A','B, C, Unclassified'),factor=2)
# ... adjust again: scaling a different category by a different factor
scenario[[1]] <- edit_scenario_template(scenario[[1]],cas_mode='car/taxi',factor=1/2)
# create the adjusted data table: 
  # it is the same as ssg.base (on which the regression is run) but it has different offset values.
  # it uses A.base, which is the baseline total travel time.
ssg.scen[[1]] <- create_scenario(scenario_table=scenario[[1]],ssg.base=ssg.base,A.base=A.base)

## scenario 2: increase female travellers
vars <- c('cas_male')
scenario[[2]] <- get_scenario_template(vars)
scenario[[2]] <- edit_scenario_template(scenario[[2]],cas_male=0,factor=2)
ssg.scen[[2]] <- create_scenario(scenario_table=scenario[[2]],ssg.base=ssg.base,A.base=A.base)

## scenario 3: decrease 16--20-year-old male car drivers
vars <- c('cas_age_band','cas_male','cas_mode')
scenario[[3]] <- get_scenario_template(vars)
scenario[[3]][1:2,2,4] <- 0.1
ssg.scen[[3]] <- create_scenario(scenario_table=scenario[[3]],ssg.base=ssg.base,A.base=A.base)

## scenario 4: decrease 20--40-year-old car drivers
vars <- c('cas_age_band','cas_mode')
scenario[[4]] <- get_scenario_template(vars)
scenario[[4]] <- edit_scenario_template(scenario[[4]],cas_age_band='[0,16)',factor=2)
ssg.scen[[4]] <- create_scenario(scenario_table=scenario[[4]],ssg.base=ssg.base,A.base=A.base)

##########################################
## PART 2: Get predictions ##

tabs <- list()
# to get predictions, use:
  # fit: the fitted regression model
  # fitted.values: fitted values from the fitted regression model
  # newdata: the dataframe generated for the scenario
  # get.se: whether or not to return also the standard error. (Cannot return standard error with available files - keep as FALSE)
tabs[[1]] <- scenario_predictions(fitted.values=fitted.values,newdata=ssg.base,get.se=FALSE)
for(i in 1:length(ssg.scen))
  tabs[[i+1]] <- scenario_predictions(fitted.values=fitted.values,newdata=ssg.scen[[i]],get.se=FALSE)

##########################################
## PART 3: Sum predictions, print results ##

results <- matrix(0,nrow=length(tabs)+1,ncol=3)
results[1,1] <- sum(tabs[[1]][tabs[[1]]$cas_mode=='pedestrian'&tabs[[1]]$strike_mode=='motorcycle',9])
results[1,2] <- sum(tabs[[1]][tabs[[1]]$cas_mode=='cyclist'&tabs[[1]]$strike_mode=='motorcycle',9])
results[1,3] <- sum(tabs[[1]][tabs[[1]]$cas_mode=='motorcycle'&tabs[[1]]$strike_mode=='car/taxi',9]) + 
  sum(tabs[[1]][tabs[[1]]$strike_mode=='motorcycle'&tabs[[1]]$cas_mode=='car/taxi',9])
for(i in 1:length(tabs)){
  results[i+1,1] <- sum(tabs[[i]][tabs[[i]]$cas_mode=='pedestrian'&tabs[[i]]$strike_mode=='motorcycle',17])
  results[i+1,2] <- sum(tabs[[i]][tabs[[i]]$cas_mode=='cyclist'&tabs[[i]]$strike_mode=='motorcycle',17])
  results[i+1,3] <- sum(tabs[[i]][tabs[[i]]$cas_mode=='motorcycle'&tabs[[i]]$strike_mode=='car/taxi',17]) + 
    sum(tabs[[i]][tabs[[i]]$strike_mode=='motorcycle'&tabs[[i]]$cas_mode=='car/taxi',17])
}
rownames(results) <- c('Data','Baseline','Scenario 1','Scenario 2','Scenario 3','Scenario 4')
colnames(results) <- c('ped. & mot.','cyc. & mot.','car & mot.')
cat('\nResults:\n')
print(results)

##########################################
## Scenario 2 in more detail ##

factors <- seq(1,2,length=50)
vars <- c('cas_male')
scenario2 <- get_scenario_template(vars)
results2.0 <- matrix(0,nrow=length(factors),ncol=6)
Ahat <- matrix(0,nrow=length(factors),ncol=3)
Acheck <- vector()
for(i in 1:length(factors)){
  scenario2.0 <- edit_scenario_template(scenario2,cas_male=0,factor=factors[i])
  ssg.scen2 <- create_scenario(scenario_table=scenario2.0,ssg.base=ssg.base,A.base=A.base)
  tabs2 <- scenario_predictions(fitted.values=fitted.values,newdata=ssg.scen2,get.se=FALSE)
  results2.0[i,1] <- sum(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle',17])
  results2.0[i,2] <- sum(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$cas_male==0,17])
  results2.0[i,3] <- sum(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$cas_male==1,17])
  results2.0[i,4] <- sum(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$cas_male==0&tabs2$strike_male==1,17])
  results2.0[i,5] <- sum(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$cas_male==1&tabs2$strike_male==0,17])
  results2.0[i,6] <- sum(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$cas_male==1&tabs2$strike_male==1,17])
  Ahat[i,1] <- mean(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle',11])
  Ahat[i,2] <- mean(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$cas_male==0,11])
  Ahat[i,3] <- mean(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$cas_male==1,11])
  Acheck[i] <- mean(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle',13])
}
cols <- c('grey','navyblue','hotpink','turquoise','purple','skyblue')
legend <- c('Total','F victim','M victim','F victim, M striker','M victim, F striker','M victim, M striker')
x11(); par(mar=c(5,5,1,1));matplot(factors,results2.0,typ='l',lwd=2,lty=1,col=cols,ylim=c(0,9600),
  frame=F,cex.axis=1.5,cex.lab=1.5,xlab='Relative female travel time',ylab='Number of motorbikes hitting pedestrians'); 
legend(x=1,y=8100,legend=legend,bty='n',col=cols,lty=1,lwd=2,cex=1.25)
x11(); par(mar=c(5,5,1,1));matplot(factors,Ahat,typ='l',lwd=2,lty=1,col=cols,ylim=c(0.6,1.6),
  frame=F,cex.axis=1.5,cex.lab=1.5,xlab='Relative female travel time',ylab='"cas_reltime" (c)'); 
lines(factors,Acheck,typ='l',lwd=3,lty=1,col='orange2'); 
legend(x=1,y=0.8,legend=c('Average',legend[2:3],'cas_reltime_group (a)'),bty='n',col=c(cols[1:3],'orange2'),lty=1,lwd=2)

##########################################
## The problem with Scenario 2 ##
cat('\nThe problem with Scenario 4:\n')
cat('\nTime-travel unit of under-16 year olds on motorbikes:\n')
cat(sum(A.base[[cas_mode='motorcycle',cas_age_band='0']]))
cat('\nInjuries caused by under-16 year olds on motorbikes:\n')
cat(sum(fitted.values[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$strike_age_band=='[0,16)']))
cat('\n\nTime-travel unit of over-16 year olds on motorbikes:\n')
cat(sum(A.base[[cas_mode='motorcycle',cas_age_band=c(2:7)]]))
cat('\nInjuries caused by over-16 year olds on motorbikes:\n')
cat(sum(fitted.values[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$strike_age_band!='[0,16)']))
cat('\n\nRate of injuries per unit time of under-16 year olds on motorbikes:\n')
cat(sum(fitted.values[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$strike_age_band=='[0,16)'])/sum(A.base[[cas_mode='motorcycle',cas_age_band='0']]))
cat('\nRate of injuries per unit time of over-16 year olds on motorbikes:\n')
cat(sum(fitted.values[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$strike_age_band!='[0,16)'])/sum(A.base[[cas_mode='motorcycle',cas_age_band=c(2:7)]]))


factors <- seq(1,2,length=50)
vars <- c('cas_age_band')
scenario2 <- get_scenario_template(vars)
Ahat <- matrix(0,nrow=length(factors),ncol=6)
Acheck <- matrix(0,nrow=length(factors),ncol=4)
for(i in 1:length(factors)){
  scenario2.0 <- edit_scenario_template(scenario2,cas_age_band='[0,16)',factor=factors[i])
  ssg.scen2 <- create_scenario(scenario_table=scenario2.0,ssg.base=ssg.base,A.base=A.base)
  tabs2 <- scenario_predictions(fitted.values=fitted.values,newdata=ssg.scen2,get.se=FALSE)
  Ahat[i,1] <- mean(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle',11])
  Ahat[i,2] <- mean(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$cas_age_band=='[0,20)',11])
  Ahat[i,3] <- mean(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle'&tabs2$cas_age_band!='[0,20)',11])
  Ahat[i,4] <- mean(tabs2[tabs2$cas_mode=='cyclist'&tabs2$strike_mode=='motorcycle',11])
  Ahat[i,5] <- mean(tabs2[tabs2$cas_mode=='cyclist'&tabs2$strike_mode=='motorcycle'&tabs2$cas_age_band=='[0,20)',11])
  Ahat[i,6] <- mean(tabs2[tabs2$cas_mode=='cyclist'&tabs2$strike_mode=='motorcycle'&tabs2$cas_age_band!='[0,20)',11])
  Acheck[i,1] <- mean(tabs2[tabs2$cas_mode=='pedestrian'&tabs2$strike_mode=='motorcycle',13])
  Acheck[i,2] <- mean(tabs2[tabs2$cas_mode=='cyclist'&tabs2$strike_mode=='motorcycle',13])
  Acheck[i,3] <- mean(tabs2[tabs2$cas_mode=='car/taxi'&tabs2$strike_mode=='motorcycle',13])
  Acheck[i,4] <- mean(tabs2[tabs2$cas_mode=='motorcycle'&tabs2$strike_mode=='car/taxi',13])
}
cols <- c('grey','navyblue','hotpink','turquoise','purple','skyblue')
legend <- c('Pedestrian','Cyclist','Car/taxi','Motorcycle')
x11(); par(mar=c(5,5,1,1));matplot(factors,Acheck,typ='l',lwd=3,lty=1,col=cols,ylim=c(1,1.25),
  frame=F,cex.axis=1.5,cex.lab=1.5,xlab='Relative travel times of 0-16 year olds',ylab='"cas_reltime_group" (a)'); 
legend(x=1,y=1.25,legend=legend,bty='n',col=c(cols),lty=1,lwd=3,cex=1.25)
cols <- c('navyblue','grey','hotpink','purple','turquoise','skyblue')
legend <- c('Pedestrian','Pedestrian < 20','Pedestrian > 20','Cyclist','Cyclist < 20','Cyclist > 20')
x11(); par(mar=c(5,5,1,1));matplot(factors,Ahat,typ='l',lwd=3,lty=1,col=cols,ylim=c(0.8,1.5),
  frame=F,cex.axis=1.5,cex.lab=1.5,xlab='Relative travel times of 0-16 year olds',ylab='"cas_reltime" (c)'); 
legend(x=1,y=1.5,legend=legend,bty='n',col=c(cols),lty=1,lwd=3,cex=1.25)



