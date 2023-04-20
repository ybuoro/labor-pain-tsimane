rm(list=ls())
library(readxl)
data <- read_excel("~/Bureau/PAIN TSIMANE REVIEWER VERSION/FILES/longitudinal_data.xlsx")

#preparation of the dataset
data$Pain<-as.numeric(data$Pain)
data$z_age<-as.numeric(data$z_age)
data$agecat<-as.character(data$agecat)
data$Pain.Days<-as.numeric(data$Pain.Days)

data$male<-factor(data$male)

data[data$agecat=="19",16]<-"10-19"
data[data$agecat=="29",16]<-"20-29"
data[data$agecat=="39",16]<-"30-39"
data[data$agecat=="49",16]<-"40-49"
data[data$agecat=="59",16]<-"50-59"
data[data$agecat=="60",16]<-"60+"
levelagecat=c("10-19","20-29","30-39","40-49","50-59","60+")

data[data$cause.rev=="old age",64]<-"Old age"
data[data$cause.rev=="reproduction",64]<-"Reproduction"
data[data$cause.rev=="illness",64]<-"Illness"
data[data$cause.rev=="social",64]<-"Social"
data[data$cause.rev=="weather",64]<-"Weather"
data[data$cause.rev=="fall / other accident",64]<-"Fall / other accident"
data[data$cause.rev=="work",64]<-"Work"

data$musc_diag=as.numeric(data$musc_diag)
data$gi_diag=as.numeric(data$gi_diag)
data$resp_diag=as.numeric(data$resp_diag)
data$genito_diag=as.numeric(data$genito_diag)
data$circulatory_diag=as.numeric(data$circulatory_diag)
data$infection_diag=as.numeric(data$infection_diag)
data$skin_diag=as.numeric(data$skin_diag)
data$z_afb=as.numeric(data$z_afb)
data$z_meanibi=as.numeric(data$z_meanibi)
data$z_totalkids=as.numeric(data$z_totalkids)
data$kidunder1=as.numeric(data$kidunder1)
data$z_age=as.numeric(data$z_age)
data$infection_diag=as.numeric(data$infection_diag)
data$skin_diag=as.numeric(data$skin_diag)

#reorder the priority of each category of anatomical location: 1) arm, 2) foot, 3) hand, 4) leg, 5) arm.
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("back","foot", "hand", "leg", "arm"))
data<-data[data$pid!="NA",]


#### TABLE S1 #####

options(digits=10)

#### A) PAIN OUTCOMES ####

### 1) % with current pain ###

## N individuals ##
n.anypain=length(unique(data$pid))
n.anypain #388 individuals in total in the dataset

datanonapain=data[!is.na(data$Pain),] #remove all person.observations with NA in Pain (only individuals with either 0 or 1 in Pain column)
length(unique(datanonapain$pid)) #still 388 individuals
## --------------------- ##

# loop on the 388 individuals
markpain=NULL #to count how many individuals have at least one observation of pain in order to get the percentage of individuals with pain
for ( i in 1:n.anypain)
{
  #i=1
  
  ttindiv=datanonapain[datanonapain$pid==unique(datanonapain$pid)[i],]
  tindiv=ttindiv[!is.na(ttindiv$Pain),]
  if(is.na(sum(tindiv$Pain)))
  {tmarkpain=0
  }
  if (!is.na(sum(tindiv$Pain)))
  {
    if (sum(tindiv$Pain)>=1)
    {tmarkpain=1
    }
    if(sum (tindiv$Pain)==0)
    {tmarkpain=0
    }
  }
  markpain=rbind(markpain,tmarkpain)
}

## Mean or % - percentage of individuals with current pain
pc.anypain= round(100*(sum(markpain)/length(unique(datanonapain$pid))),2) #divide number of individuals with pain (stored in 'markpain') by the total number of individuals
## --------------------- ##

## SD ##
sd.anypain=round(sd(markpain),2)
## --------------------- ##

## MAX ##
max.anypain=round(max(markpain),2)
## --------------------- ##

## MIN ##
min.anypain=round(min(markpain),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 2) Pain duration ###

datanonapd=data[!is.na(data$Pain.Days),] #remove all person.observations with a NA in Pain.Days 

datanonapd$Pain.Days<-as.numeric(datanonapd$Pain.Days) 

#total number of individuals, but some individuals do not have complete pain data on all anatomical locations
tn.pd=length(unique(datanonapd$pid))

#loop 1
markpain=NULL #to count the number of individuals with complete pain data on all anatomical locations
mark.pid=NULL # to store the pids that have complete pain data on all anatomical locations (used in loop 2)

for (i in 1:tn.pd)
{
  #i=1
  
  ttindiv = datanonapd[datanonapd$pid == unique(datanonapd$pid)[i], ]
  tindiv=ttindiv[!is.na(ttindiv$Pain.Days),]
  
  if (!is.na(sum(tindiv$Pain.Days)))
  {
    if (length(unique(tindiv$Anatomical.location)) == 5)
    {
      if (sum(tindiv$Pain.Days) >= 0)
      {
        tmarkpain = 1
        markpain = c(markpain, tmarkpain)
        
        tmark.pid=unique(tindiv$pid)
        mark.pid=c(tmark.pid, mark.pid)
      }
    }
  }
}

## N individuals ##
n.pd=sum(markpain) #total number of individuals with data on all 5 anatomical locations

# loop 2
ana.loc=unique(datanonapd$Anatomical.location) #storing the names of the 5 anatomical locations
sumpain=NULL #storing the maximum value of pain duration for the individual
n.pobs=NULL # storing the number of person-observation for each individual
for (i in 1:n.pd)
{
  #i=140
  ttindiv = datanonapd[datanonapd$pid == mark.pid[i],]
  tindiv=ttindiv[!is.na(ttindiv$Pain.Days),]
  
  tsumpain = max(tindiv$Pain.Days)
  sumpain = c(tsumpain, sumpain)
}
## --------------------- ##

## Mean or % - mean duration of pain (days) ##
mean.painpd=round(sum(sumpain)/n.pd,2)
## --------------------- ##

## SD ##
sd.painpd=round(sd(sumpain),2)
## --------------------- ##

## MAX ##
max.painpd=round(max(sumpain),2)
## --------------------- ##

## MIN ##
min.painpd=round(min(sumpain),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 3) % with chronic pain (≥ 3 months)
datanonapd$Chronic.Pain90<-as.numeric(datanonapd$Chronic.Pain90)
tdata.90d=datanonapd[datanonapd$Pain.Days!=0,] # Sample size is reduced due to missing pain duration data at certain anatomical locations.
data.90d=tdata.90d[!is.na(tdata.90d$Chronic.Pain90),]  # remove all person.observations with a NA in Chronic Pain >= 3 months

## N individuals ##
n.pain90d=length(unique(data.90d$pid))
## --------------------- ##

#loop
markpain=NULL #to store individuals that have chronic pain (≥90 days)
for (i in 1:n.pain90d)
{
  tindiv = data.90d[data.90d$pid == unique(data.90d$pid)[i], ]
  
  if (!is.na(sum(tindiv$Chronic.Pain90)))
  {
    if (sum(tindiv$Chronic.Pain90) >= 1)
    {
      tmarkpain = 1
    }
    if (sum (tindiv$Chronic.Pain90) == 0)
    {
      tmarkpain = 0
    }
  }
  markpain = rbind(markpain, tmarkpain)
}

## Mean or % - percentage of individuals with chronic pain (≥90 days)
pc.pain90d= round(100*(sum(markpain)/n.pain90d),2)
## --------------------- ##

## SD ##
sd.pain90d=round(sd(markpain),2)
## --------------------- ##

## MAX ##
max.pain90d=1
## --------------------- ##

## MIN ##
min.pain90d=0
### ---------------------------------------------------------------------------------------------------------------------- ###


### 4) % with chronic pain (≥ 6 months) ###
#Sample size is reduced due to missing pain duration data at certain anatomical locations.
datanonapd$Chronic.Pain180<-as.numeric(datanonapd$Chronic.Pain180)
tdata.180d=datanonapd[datanonapd$Pain.Days!=0,] # Sample size is reduced due to missing pain duration data at certain anatomical locations.
data.180d=tdata.180d[!is.na(tdata.180d$Chronic.Pain180),]

## N individuals ##
n.pain180d=length(unique(data.180d$pid))
## --------------------- ##

#loop
markpain=NULL #to store individuals that have chronic pain (≥ 6 months |180 days)
for ( i in 1:n.pain180d)
{
  tindiv=data.180d[data.180d$pid==unique(data.180d$pid)[i],]
  
  if (!is.na(sum(tindiv$Chronic.Pain180)))
  {
    if (sum(tindiv$Chronic.Pain180)>=1)
    {
      tmarkpain=1
    }
    if(sum (tindiv$Chronic.Pain180)==0)
    {
      tmarkpain=0
    }
  }
  markpain=rbind(markpain,tmarkpain)
}

## Mean or % - percentage of individuals with chronic pain (≥180 days)
pc.pain180d= round(100*(sum(markpain)/n.pain180d),2)
## --------------------- ##

## SD ##
sd.pain180d=round(sd(markpain),2)
## --------------------- ##

## MAX ##
max.pain180d=1
## --------------------- ##

## MIN ##
min.pain180d=0
### ---------------------------------------------------------------------------------------------------------------------- ###

### 5) Pain outcomes table ###
outcomeorpred.po=c("% with current pain", "Pain duration (days)", "% with chronic pain (≥3 months)", "% with chronic pain (≥6 months)") #column names for the tibble
N.po=c(n.anypain, n.pd, n.pain90d, n.pain180d) # total numbers of individuals
meanorpc.po=c(pc.anypain,  mean.painpd, pc.pain90d, pc.pain180d) # mean or %
sd.po=c(sd.anypain, sd.painpd, sd.pain90d, sd.pain180d) # SD
min.po=c(min.anypain, min.painpd, min.pain90d, min.pain180d) # Min
max.po=c(max.anypain, max.painpd, max.pain90d, max.pain180d) # Max

library(tibble)
pain.outcome=tibble('Outcome or predictor'= outcomeorpred.po, N = N.po, 'Mean or percent'=meanorpc.po, SD=sd.po, Min=min.po, Max=max.po)
#### --------------------------------------------------------------------------------------------------------------------------------------------------- ####


#### B) SOCIO-DEMOGRAPHICS ####

### 1) Age (years) ###

## N individuals ##
n.age=length(unique(data$pid))
## --------------------- ##

#remove lines with na in pain for person.obs
data$age<-as.numeric(data$age)
datanonaage=data[!is.na(data$age),]

## Mean or % - mean age in the dataset
mean.age=round(sum(datanonaage$age)/dim(data)[1],2)
## --------------------- ##

## SD ##
sd.age=round(sd(datanonaage$age),2) #not 14.04
## --------------------- ##

## MAX ##
max.age=round(max(datanonaage$age),2)
## --------------------- ##

## MIN ##
min.age=round(min(datanonaage$age),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 2) % male ###

datamale=datanonaage[datanonaage$male==1,]

## N individuals ##
n.male=length(unique(datamale$pid))
## --------------------- ##

## Mean or % ##
pc.male=round(100*(n.male/n.age),2)
## --------------------- ##

##SD
sd.male=round(sd(datamale$age),2)
## --------------------- ##

## MAX ##
max.male=round(max(datamale$age),2)
## --------------------- ##

## MIN ##
min.male=round(min(datamale$age),2)
### ----------------------- ###


### 3) Years of schooling ###

datayschool=data[!is.na(data$YearsSchool),]

## N individuals ##
n.yschool=length(unique(datayschool$pid))
## --------------------- ##

## Mean or % ##
mean.yschool=round(mean(datayschool$YearsSchool),2)
## --------------------- ##

##SD
sd.yschool=round(sd(datayschool$YearsSchool),2)
## --------------------- ##

## MAX ##
max.yschool=round(max(datayschool$YearsSchool),2)
## --------------------- ##

## MIN ##
min.yschool=round(min(datayschool$YearsSchool),2)
### ----------------------- ###

### 3)  Socio-demographics table ###
outcomeorpred.sd=c("Age (years)", "% male", "Years of schooling")
N.sd=c(n.age, n.age, n.yschool)
meanorpc.sd=c(mean.age,pc.male, mean.yschool)
sd.sd=c(sd.age,sd.male, sd.yschool)
min.sd=c(min.age,min.male, min.yschool)
max.sd=c(max.age,max.male, max.yschool)
# row combined
library(tibble)
sd.outcome=tibble('Outcome or predictor'= outcomeorpred.sd, N = N.sd, 'Mean or percent'=meanorpc.sd, SD=sd.sd, Min=min.sd, Max=max.sd)
#### --------------------------------------------------------------------------------------------------------------------------------------------------- ####


#### C) HEALTH INDICATORS ####

### 1) % respiratory ###
data$resp_diag<-as.numeric(data$resp_diag)
dataresp_diag=data[!is.na(data$resp_diag),]

## N individuals ##
n.resp_diag=length(unique(dataresp_diag$pid))
## --------------------- ##


markresp_diag=NULL
minmaxresp_diag=NULL

for ( i in 1:n.resp_diag)
{
  tindiv=dataresp_diag[dataresp_diag$pid==unique(dataresp_diag$pid)[i],]
  if(is.na(sum(tindiv$resp_diag)))
  {tmarkresp_diag=0
  tminmaxresp_diag=0
  }
  if (!is.na(sum(tindiv$resp_diag)))
  {
    if (sum(tindiv$resp_diag)>=1)
    {tmarkresp_diag=1
    tminmaxresp_diag=max(tindiv$resp_diag)
    }
    if(sum (tindiv$resp_diag)==0)
    {tmarkresp_diag=0
    tminmaxresp_diag=0
    }
  }
  minmaxresp_diag=rbind(minmaxresp_diag, tminmaxresp_diag)
  markresp_diag=rbind(markresp_diag,tmarkresp_diag)
}
sum(markresp_diag)
sum(markresp_diag)/n.resp_diag

## Mean or %
pc.resp_diag=round(sum(markresp_diag)/n.resp_diag,2)*100
## --------------------- ##

## SD ##
sd.resp_diag=round(sd(dataresp_diag$resp_diag),2)
## --------------------- ##

## MIN ##
min.resp_diag=round(min(dataresp_diag$resp_diag),2)
## --------------------- ##

## MAX ##
max.resp_diag=round(max(dataresp_diag$resp_diag),2)
### ---------------------------------------------------------------------------------------------------------------------- ###

### 2) % musculoskeletal ###
data$musc_diag<-as.numeric(data$musc_diag)
datamusc_diag=data[!is.na(data$musc_diag),]

## N individuals 
n.musc_diag=length(unique(datamusc_diag$pid))
## --------------------- ##

markmusc_diag=NULL
minmaxmusc_diag=NULL

for ( i in 1:n.musc_diag)
{
  tindiv=datamusc_diag[datamusc_diag$pid==unique(datamusc_diag$pid)[i],]
  if(is.na(sum(tindiv$musc_diag)))
  {tmarkmusc_diag=0
  tminmaxmusc_diag=0
  }
  if (!is.na(sum(tindiv$musc_diag)))
  {
    if (sum(tindiv$musc_diag)>=1)
    {tmarkmusc_diag=1
    tminmaxmusc_diag=max(tindiv$musc_diag)
    }
    if(sum (tindiv$musc_diag)==0)
    {tmarkmusc_diag=0
    tminmaxmusc_diag=0
    }
  }
  minmaxmusc_diag=rbind(minmaxmusc_diag, tminmaxmusc_diag)
  markmusc_diag=rbind(markmusc_diag,tmarkmusc_diag)
}
sum(markmusc_diag)
sum(markmusc_diag)/n.musc_diag

## Mean or % ##
pc.musc_diag=round(sum(markmusc_diag)/n.musc_diag,2)*100#CORRECT
## --------------------- ##

## SD ##
sd.musc_diag=round(sd(datamusc_diag$musc_diag),2)
## --------------------- ##

## MIN ##
min.musc_diag=round(min(datamusc_diag$musc_diag),2)
## --------------------- ##

## MAX ##
max.musc_diag=round(max(datamusc_diag$musc_diag),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 3) % gastrointestinal ###

data$gi_diag<-as.numeric(data$gi_diag)
datagi_diag=data[!is.na(data$gi_diag),]

## N individuals 
n.gi_diag=length(unique(datagi_diag$pid))
## --------------------- ##

markgi_diag=NULL
minmaxgi_diag=NULL

for ( i in 1:n.gi_diag)
{
  tindiv=datagi_diag[datagi_diag$pid==unique(datagi_diag$pid)[i],]
  if(is.na(sum(tindiv$gi_diag)))
  {tmarkgi_diag=0
  tminmaxgi_diag=0
  }
  if (!is.na(sum(tindiv$gi_diag)))
  {
    if (sum(tindiv$gi_diag)>=1)
    {tmarkgi_diag=1
    tminmaxgi_diag=max(tindiv$gi_diag)
    }
    if(sum (tindiv$gi_diag)==0)
    {tmarkgi_diag=0
    tminmaxgi_diag=0
    }
  }
  minmaxgi_diag=rbind(minmaxgi_diag, tminmaxgi_diag)
  markgi_diag=rbind(markgi_diag,tmarkgi_diag)
}
sum(markgi_diag)
sum(markgi_diag)/n.gi_diag

## Mean or % ##
pc.gi_diag=round(sum(markgi_diag)/n.gi_diag,2)*100
## --------------------- ##

## SD ##
sd.gi_diag=round(sd(datagi_diag$gi_diag),2)
## --------------------- ##

## MIN ##
min.gi_diag=round(min(datagi_diag$gi_diag),2)
## --------------------- ##

## MAX ##
max.gi_diag=round(max(datagi_diag$gi_diag),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 4) % genitourinary ###

data$genito_diag<-as.numeric(data$genito_diag)
datagenito_diag=data[!is.na(data$genito_diag),]

## N individuals ##
n.genito_diag=length(unique(datagenito_diag$pid))#CORRECT
## --------------------- ##


markgenito_diag=NULL
minmaxgenito_diag=NULL

for ( i in 1:n.genito_diag)
{
  tindiv=datagenito_diag[datagenito_diag$pid==unique(datagenito_diag$pid)[i],]
  if(is.na(sum(tindiv$genito_diag)))
  {tmarkgenito_diag=0
  tminmaxgenito_diag=0
  }
  if (!is.na(sum(tindiv$genito_diag)))
  {
    if (sum(tindiv$genito_diag)>=1)
    {tmarkgenito_diag=1
    tminmaxgenito_diag=max(tindiv$genito_diag)
    }
    if(sum (tindiv$genito_diag)==0)
    {tmarkgenito_diag=0
    tminmaxgenito_diag=0
    }
  }
  minmaxgenito_diag=rbind(minmaxgenito_diag, tminmaxgenito_diag)
  markgenito_diag=rbind(markgenito_diag,tmarkgenito_diag)
}
sum(markgenito_diag)
sum(markgenito_diag)/n.genito_diag

## Mean or % ##
pc.genito_diag=round(sum(markgenito_diag)/n.genito_diag,2)*100#
## --------------------- ##

## SD ##
sd.genito_diag=round(sd(datagenito_diag$genito_diag),2)
## --------------------- ##

## MIN ##
min.genito_diag=round(min(datagenito_diag$genito_diag),2)
## --------------------- ##

## MAX ##
max.genito_diag=round(max(datagenito_diag$genito_diag),2)
## --------------------- ##


### 5) % any other infections ###

data$infection_diag<-as.numeric(data$infection_diag)
datainfection_diag=data[!is.na(data$infection_diag),]

## N individuals ##
n.infection_diag=length(unique(datainfection_diag$pid))
## --------------------- ##


markinfection_diag=NULL
minmaxinfection_diag=NULL

for ( i in 1:n.infection_diag)
{
  tindiv=datainfection_diag[datainfection_diag$pid==unique(datainfection_diag$pid)[i],]
  if(is.na(sum(tindiv$infection_diag)))
  {tmarkinfection_diag=0
  tminmaxinfection_diag=0
  }
  if (!is.na(sum(tindiv$infection_diag)))
  {
    if (sum(tindiv$infection_diag)>=1)
    {tmarkinfection_diag=1
    tminmaxinfection_diag=max(tindiv$infection_diag)
    }
    if(sum (tindiv$infection_diag)==0)
    {tmarkinfection_diag=0
    tminmaxinfection_diag=0
    }
  }
  minmaxinfection_diag=rbind(minmaxinfection_diag, tminmaxinfection_diag)
  markinfection_diag=rbind(markinfection_diag,tmarkinfection_diag)
}
sum(markinfection_diag)
sum(markinfection_diag)/n.infection_diag

## Mean or % ##
pc.infection_diag=round(sum(markinfection_diag)/n.infection_diag,2)*100
## --------------------- ##

## SD ##
sd.infection_diag=round(sd(datainfection_diag$infection_diag),2)
## --------------------- ##

## MIN ##
min.infection_diag=round(min(datainfection_diag$infection_diag),2)
## --------------------- ##

## MAX ##
max.infection_diag=round(max(datainfection_diag$infection_diag),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 6) % circulatory ###

data$circulatory_diag<-as.numeric(data$circulatory_diag)
datacirculatory_diag=data[!is.na(data$circulatory_diag),]

## N individuals ##
n.circulatory_diag=length(unique(datacirculatory_diag$pid))
## --------------------- ##


markcirculatory_diag=NULL
minmaxcirculatory_diag=NULL

for ( i in 1:n.circulatory_diag)
{
  tindiv=datacirculatory_diag[datacirculatory_diag$pid==unique(datacirculatory_diag$pid)[i],]
  if(is.na(sum(tindiv$circulatory_diag)))
  {tmarkcirculatory_diag=0
  tminmaxcirculatory_diag=0
  }
  if (!is.na(sum(tindiv$circulatory_diag)))
  {
    if (sum(tindiv$circulatory_diag)>=1)
    {tmarkcirculatory_diag=1
    tminmaxcirculatory_diag=max(tindiv$circulatory_diag)
    }
    if(sum (tindiv$circulatory_diag)==0)
    {tmarkcirculatory_diag=0
    tminmaxcirculatory_diag=0
    }
  }
  minmaxcirculatory_diag=rbind(minmaxcirculatory_diag, tminmaxcirculatory_diag)
  markcirculatory_diag=rbind(markcirculatory_diag,tmarkcirculatory_diag)
}
sum(markcirculatory_diag)
sum(markcirculatory_diag)/n.circulatory_diag

## Mean or % ##
pc.circulatory_diag=round(sum(markcirculatory_diag)/n.circulatory_diag,2)*100
## --------------------- ##

## SD ##
sd.circulatory_diag=round(sd(datacirculatory_diag$circulatory_diag),2)
## --------------------- ##

## MIN ##
min.circulatory_diag=round(min(datacirculatory_diag$circulatory_diag),2)
## --------------------- ##

## MAX ##
max.circulatory_diag=round(max(datacirculatory_diag$circulatory_diag),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 7) % skin/subcutaneous tissue ###

data$skin_diag<-as.numeric(data$skin_diag)
dataskin_diag=data[!is.na(data$skin_diag),]

## N individuals ##
n.skin_diag=length(unique(dataskin_diag$pid))
## --------------------- ##


markskin_diag=NULL
minmaxskin_diag=NULL

for ( i in 1:n.skin_diag)
{
  #i=1
  
  tindiv=dataskin_diag[dataskin_diag$pid==unique(dataskin_diag$pid)[i],]
  if(is.na(sum(tindiv$skin_diag)))
  {tmarkskin_diag=0
  tminmaxskin_diag=0
  }
  if (!is.na(sum(tindiv$skin_diag)))
  {
    if (sum(tindiv$skin_diag)>=1)
    {tmarkskin_diag=1
    tminmaxskin_diag=max(tindiv$skin_diag)
    }
    if(sum (tindiv$skin_diag)==0)
    {tmarkskin_diag=0
    tminmaxskin_diag=0
    }
  }
  minmaxskin_diag=rbind(minmaxskin_diag, tminmaxskin_diag)
  markskin_diag=rbind(markskin_diag,tmarkskin_diag)
}
sum(markskin_diag)
sum(markskin_diag)/n.skin_diag

## Mean or % ##
pc.skin_diag=round(sum(markskin_diag)/n.skin_diag,2)*100
## --------------------- ##

## SD ##
sd.skin_diag=round(sd(dataskin_diag$skin_diag),2)
## --------------------- ##

## MIN ##
min.skin_diag=round(min(dataskin_diag$skin_diag),2)
## --------------------- ##

## MAX ##
max.skin_diag=round(max(dataskin_diag$skin_diag),2)
## --------------------- ##


### 8) Sum of conditions ###
data$sumdiags<-as.numeric(data$sumdiags)
datasumdiags=data[!is.na(data$sumdiags), ]

## N individuals 
n.sumdiags=length(unique(datasumdiags$pid))
## --------------------- ##

## Mean or % ##
mean.sumdiags=round(mean(datasumdiags$sumdiags),2)
## --------------------- ##

## SD ##
sd.sumdiags=round(sd(datasumdiags$sumdiags),2)# 0.76
## --------------------- ##

## MIN ##
min.sumdiags=round(min(datasumdiags$sumdiags),2)
## --------------------- ##

## MAX ##
max.sumdiags=round(max(datasumdiags$sumdiags),2)
### ---------------------------------------------------------------------------------------------------------------------- ###

### 9) Health indicators table ###
outcomeorpred.hi=c( "% with respiratory condition","% with musculoskeletal condition","% with gastrointestinal condition","% with genitourinatory condition", "% with any other infection", "% with circulatory condition", "% with skin/subcutaeous tissue condition", "Sum of all conditions")
N.hi=c(n.resp_diag, n.musc_diag, n.gi_diag, n.genito_diag, n.infection_diag, n.circulatory_diag, n.skin_diag, n.sumdiags)
meanorpc.hi=c(pc.resp_diag, pc.musc_diag, pc.gi_diag, pc.genito_diag, pc.infection_diag, pc.circulatory_diag, pc.skin_diag, mean.sumdiags)
sd.hi=c(sd.resp_diag, sd.musc_diag, sd.gi_diag, sd.genito_diag, sd.infection_diag, sd.circulatory_diag, sd.skin_diag, sd.sumdiags)
min.hi=c(min.resp_diag, min.musc_diag, min.gi_diag, min.genito_diag, min.infection_diag, min.circulatory_diag, min.skin_diag, min.sumdiags)
max.hi=c(max.resp_diag, max.musc_diag, max.gi_diag, max.genito_diag, max.infection_diag, max.circulatory_diag, max.skin_diag, max.sumdiags)
library(tibble)
health.outcome=tibble('Outcome or predictor'= outcomeorpred.hi, N = N.hi, 'Mean or percent'=meanorpc.hi, SD=sd.hi, Min=min.hi, Max=max.hi)
#### --------------------------------------------------------------------------------------------------------------------------------------------------- ####


#### D) VITAL SIGNS ####

### 1) Pulse rate ###

data$fc<-as.numeric(data$fc)
datafc=data[!is.na(data$fc),]

## N individuals ## 
n.fc=length(unique(datafc$pid))
## --------------------- ##


markfc=NULL
valfc=NULL

for ( i in 1:n.fc)
{
  #i=1
  
  tindiv=datafc[datafc$pid==unique(datafc$pid)[i],]
  if(is.na(sum(tindiv$fc)))
  {tmarkfc=0
  tvalfc=0
  }
  if (!is.na(sum(tindiv$fc)))
  {
    
    tmarkfc=1
    tvalfc=unique(tindiv$fc)[1]
    
  }
  valfc=rbind(valfc, tvalfc)
  markfc=rbind(markfc,tmarkfc)
}

## Mean or % ##
mean.fc=round(mean(valfc),2)
## --------------------- ##

## SD ##
sd.fc=round(sd(valfc),2)
## --------------------- ##

## MIN ##
min.fc = round(min(datafc$fc),2)
## --------------------- ##

## MAX ##
max.fc = round(max(datafc$fc),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 2) Breathing rate ###

data$fr<-as.numeric(data$fr)
datafr=data[!is.na(data$fr),]

## N individuals ##
n.fr=length(unique(datafr$pid))
## --------------------- ##

markfr=NULL
valfr=NULL

for ( i in 1:n.fr)
{
  #i=1
  
  tindiv=datafr[datafr$pid==unique(datafr$pid)[i],]
  if(is.na(sum(tindiv$fr)))
  {tmarkfr=0
  tvalfr=0
  }
  if (!is.na(sum(tindiv$fr)))
  {
    
    tmarkfr=1
    tvalfr=unique(tindiv$fr)[1]
    
  }
  valfr=rbind(valfr, tvalfr)
  markfr=rbind(markfr,tmarkfr)
}

## Mean or % ##
mean.fr=round(mean(valfr),2)
## --------------------- ##

## SD ##
sd.fr=round(sd(valfr),2)
## --------------------- ##

## MIN ##
min.fr = round(min(datafr$fr),2)
## --------------------- ##

## MAX ##
max.fr = round(max(datafr$fr),2) 
### ---------------------------------------------------------------------------------------------------------------------- ###


### 3) Systolic blood pressure ### 

data$pasys
data$pasys<-as.numeric(data$pasys)
datapasys=data[!is.na(data$pasys),]

## N individuals ##
n.pasys=length(unique(datapasys$pid))
## --------------------- ##

markpasys=NULL
valpasys=NULL

for ( i in 1:n.pasys)
{
  #i=1
  
  tindiv=datapasys[datapasys$pid==unique(datapasys$pid)[i],]
  if(is.na(sum(tindiv$pasys)))
  {tmarkpasys=0
  tvalpasys=0
  }
  if (!is.na(sum(tindiv$pasys)))
  {
    
    tmarkpasys=1
    tvalpasys=unique(tindiv$pasys)[1]
    
  }
  valpasys=rbind(valpasys, tvalpasys)
  markpasys=rbind(markpasys,tmarkpasys)
}

## Mean or % ##
mean.pasys=round(mean(valpasys),2)
## --------------------- ##

## SD ##
sd.pasys=round(sd(valpasys),2)
## --------------------- ##

## MIN ##
min.pasys = round(min(datapasys$pasys),2)
## --------------------- ##

## MAX ##
max.pasys = round(max(datapasys$pasys),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 4) Diastolic blood pressure ###

data$padias<-as.numeric(data$padias)
datapadias=data[!is.na(data$padias),]

## N individuals ##
n.padias=length(unique(datapadias$pid))
## --------------------- ##

markpadias=NULL
valpadias=NULL

for ( i in 1:n.padias)
{
  #i=1
  
  tindiv=datapadias[datapadias$pid==unique(datapadias$pid)[i],]
  if(is.na(sum(tindiv$padias)))
  {tmarkpadias=0
  tvalpadias=0
  }
  if (!is.na(sum(tindiv$padias)))
  {
    
    tmarkpadias=1
    tvalpadias=unique(tindiv$padias)[1]
    
  }
  valpadias=rbind(valpadias, tvalpadias)
  markpadias=rbind(markpadias,tmarkpadias)
}

## Mean or % ##
mean.padias=round(mean(valpadias),2)
## --------------------- ##

## SD ##
sd.padias=round(sd(valpadias),2)
## --------------------- ##

## MIN ##
min.padias = round(min(datapadias$padias),2)
## --------------------- ##

## MAX ##
max.padias = round(max(datapadias$padias),2)
### ---------------------------------------------------------------------------------------------------------------------- ###

### 5) Vital signs table ###
outcomeorpred.vs=c("Pulse rate (beats/min)", "Breathing rate (breaths/min)","Systolic blood pressure (mmHg)","Diastolic blood pressure (mmHg)")
N.vs=c(n.fc, n.fr, n.pasys, n.padias)
meanorpc.vs=c(mean.fc, mean.fr, mean.pasys, mean.padias)
sd.vs=c(sd.fc, sd.fr, sd.pasys, sd.padias)
min.vs=c(min.fc, min.fr, min.pasys, min.padias)
max.vs=c(max.fc, max.fr, max.pasys, max.padias)
library(tibble)
vitals.outcome=tibble('Outcome or predictor'= outcomeorpred.vs, N = N.vs, 'Mean or percent'=meanorpc.vs, SD=sd.vs, Min=min.vs, Max=max.vs)
#### --------------------------------------------------------------------------------------------------------------------------------------------------- ####


#### E) INDICATORS OF FEMALE REPRODUCTIVE EFFORT ####
datafemale=data[data$male=="0",]
datamale=data[data$male=="1",]

### 1) Age at first birth ###

datafemale$afb<-as.numeric(datafemale$afb)
dataafb=datafemale[!is.na(datafemale$afb),]

## N individuals ##
n.afb=length(unique(dataafb$pid))
## --------------------- ##

markafb=NULL
valafb=NULL

for ( i in 1:n.afb)
{
  #i=1
  
  tindiv=dataafb[dataafb$pid==unique(dataafb$pid)[i],]
  if(is.na(sum(tindiv$afb)))
  {tmarkafb=0
  tvalafb=0
  }
  if (!is.na(sum(tindiv$afb)))
  {
    
    tmarkafb=1
    tvalafb=unique(tindiv$afb)[1]
    
  }
  valafb=rbind(valafb, tvalafb)
  markafb=rbind(markafb,tmarkafb)
}

### Mean or % ###
mean.afb=round(mean(valafb),2)
## --------------------- ##

## SD ##
sd.afb=round(sd(valafb),2) 
## --------------------- ##

## MIN ##
min.afb = round(min(dataafb$afb),2)
## --------------------- ##

## MAX ##
max.afb = round(max(dataafb$afb),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 2) Mean interval birth (months) ###

datafemale$mean_ibi_mos<-as.numeric(datafemale$mean_ibi_mos)
datamean_ibi_mos=datafemale[!is.na(datafemale$mean_ibi_mos),]

## N individuals ##
n.mean_ibi_mos=length(unique(datamean_ibi_mos$pid))
## --------------------- ##

markmean_ibi_mos=NULL
valmean_ibi_mos=NULL

for ( i in 1:n.mean_ibi_mos)
{
  #i=1
  
  tindiv=datamean_ibi_mos[datamean_ibi_mos$pid==unique(datamean_ibi_mos$pid)[i],]
  if(is.na(sum(tindiv$mean_ibi_mos)))
  {tmarkmean_ibi_mos=0
  tvalmean_ibi_mos=0
  }
  if (!is.na(sum(tindiv$mean_ibi_mos)))
  {
    
    tmarkmean_ibi_mos=1
    tvalmean_ibi_mos=unique(tindiv$mean_ibi_mos)[1]
    
  }
  valmean_ibi_mos=rbind(valmean_ibi_mos, tvalmean_ibi_mos)
  markmean_ibi_mos=rbind(markmean_ibi_mos,tmarkmean_ibi_mos)
}

## Mean or % ##
mean.mean_ibi_mos=round(mean(valmean_ibi_mos),2)
## --------------------- ##

## SD ##
sd.mean_ibi_mos=round(sd(valmean_ibi_mos),2)
## --------------------- ##

## MIN ##
min.mean_ibi_mos = round(min(datamean_ibi_mos$mean_ibi_mos),2)
## --------------------- ##

## MAX ##
max.mean_ibi_mos = round(max(datamean_ibi_mos$mean_ibi_mos),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 3) Total number of live birth ###

datafemale$totalkids<-as.numeric(datafemale$totalkids)
datatotalkids=datafemale[!is.na(datafemale$totalkids),]

## N individuals ##
n.totalkids=length(unique(datatotalkids$pid))
## --------------------- ##

marktotalkids=NULL
valtotalkids=NULL

for ( i in 1:n.totalkids)
{
  #i=1
  
  tindiv=datatotalkids[datatotalkids$pid==unique(datatotalkids$pid)[i],]
  if(is.na(sum(tindiv$totalkids)))
  {tmarktotalkids=0
  tvaltotalkids=0
  }
  if (!is.na(sum(tindiv$totalkids)))
  {
    
    tmarktotalkids=1
    tvaltotalkids=unique(tindiv$totalkids)[1]
    
  }
  valtotalkids=rbind(valtotalkids, tvaltotalkids)
  marktotalkids=rbind(marktotalkids,tmarktotalkids)
}

## Mean or % ##
mean.totalkids=round(mean(valtotalkids),2)
## --------------------- ##

## SD ##
sd.totalkids=round(sd(valtotalkids),2)
## --------------------- ##

## MIN ##
min.totalkids = round(min(datatotalkids$totalkids),2)
## --------------------- ##

## MAX ##
max.totalkids = round(max(datatotalkids$totalkids),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 4) % giving birth in past year ###

datafemale$kidunder1<-as.numeric(datafemale$kidunder1)
datakidunder1=datafemale[!is.na(datafemale$kidunder1),]

## N individuals ##
n.kidunder1=length(unique(datakidunder1$pid))
## --------------------- ##

markkidunder1=NULL
valkidunder1=NULL

for ( i in 1:n.kidunder1)
{
  #i=1
  
  tindiv=datakidunder1[datakidunder1$pid==unique(datakidunder1$pid)[i],]
  if(is.na(sum(tindiv$kidunder1)))
  {tmarkkidunder1=0
  tvalkidunder1=0
  }
  if (!is.na(sum(tindiv$kidunder1)))
  {
    tmarkkidunder1=1
    tvalkidunder1=unique(tindiv$kidunder1)[1]
  }
  valkidunder1=rbind(valkidunder1, tvalkidunder1)
  markkidunder1=rbind(markkidunder1,tmarkkidunder1)
}

## Mean or % ##
mean.kidunder1=round(mean(valkidunder1),2)*100
## --------------------- ##

## SD ##
sd.kidunder1=round(sd(valkidunder1),2)
## --------------------- ##

## MIN ##
min.kidunder1 = round(min(datakidunder1$kidunder1),2)
## --------------------- ##

## MAX ##
max.kidunder1 = round(max(datakidunder1$kidunder1),2)
### ---------------------------------------------------------------------------------------------------------------------- ###


### 5) Female reproductive effort table  ###
outcomeorpred.fre=c("Age at first birth (years)", "Mean interbirth interval (months)","Total number of live births","% giving birth in past year")
N.fre=c(n.afb, n.mean_ibi_mos, n.totalkids, n.kidunder1)
meanorpc.fre=c(mean.afb, mean.mean_ibi_mos, mean.totalkids, mean.kidunder1)
sd.fre=c(sd.afb, sd.mean_ibi_mos, sd.totalkids, sd.kidunder1)
min.fre=c(min.afb, min.mean_ibi_mos, min.totalkids, min.kidunder1)
max.fre=c(max.afb, max.mean_ibi_mos, max.totalkids, max.kidunder1)
library(tibble)
fre.outcome=tibble('Outcome or predictor'= outcomeorpred.fre, N = N.fre, 'Mean or percent'=meanorpc.fre, SD=sd.fre, Min=min.fre, Max=max.fre)
### ---------------------------------------------------------------------------------------------------------------------- ###

#### Fusing all the tables ###
tpainoutcome=tibble('Outcome or predictor'= "Pain outcomes", N = " ", 'Mean or percent'= " ", SD= " ", Min= " ", Max=" ")
tsdoutcome=tibble('Outcome or predictor'= "Socio-demographics", N = " ", 'Mean or percent'= " ", SD= " ", Min= " ", Max=" ")
thealthoutcome=tibble('Outcome or predictor'= "Health indicators: Clinical diagnosis categories", N = " ", 'Mean or percent'= " ", SD= " ", Min= " ", Max=" ")
tvitalsoutcome=tibble('Outcome or predictor'= "Health indicators: Vitals signs", N = " ", 'Mean or percent'= " ", SD= " ", Min= " ", Max=" ")
tfreoutcome=tibble('Outcome or predictor'= "Indicators of female reproductive effort", N = " ", 'Mean or percent'= " ", SD= " ", Min= " ", Max=" ")

table.s1=rbind(tpainoutcome, pain.outcome, tsdoutcome, sd.outcome, thealthoutcome, health.outcome, tvitalsoutcome, vitals.outcome, tfreoutcome, fre.outcome)
table.s1
format(table.s1, digits=2, nsmall=2)
write.csv(table.s1, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS1.csv")
######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE 1 ######### 

#ddata is a dataset without NA that will be used to get the number of person-observations for each age categories and genders, and to compute the bars in the ggplot.
ddata=data[!data$cause.rev=="NA",]
library(dplyr)
tdlabdata<-arrange(ddata, factor(agecat, levels=levelagecat))
tdlab.male=tdlabdata[tdlabdata$male=='1',]
tdlab.female=tdlabdata[tdlabdata$male=='0',]

## 1) Percentage of pain by age category and gender ##
#datasets for the denominators in the calculus of percentages of pain for each age category and gender
data.male=data[data$male=="1",]
data.female=data[data$male=="0",]

#ensuring that no NA values are left in tdata.male and tdata.female, that will be used in calculating the percentages of pain for each age category and gender.
tdata=data[data$Pain=="1",]
ttdata.male=tdata[tdata$male=="1",]
ttdata.female=tdata[tdata$male=="0",]
tdata.male=ttdata.male[!is.na(ttdata.male$pid),]
tdata.female=ttdata.female[!is.na(ttdata.female$pid),]


tdlab.male=tibble(tdlab.male, pcpain="NA") # will be used to store male percentages of pain for each age category

# computing percentage of individuals having pain: all the PIDs that have pain for the same age category and sex, divided by all the PIDs of the individuals of the same age category and sex
tdlab.male[tdlab.male$agecat=="10-19",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="10-19", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="10-19", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="20-29",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="20-29", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="20-29", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="30-39",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="30-39", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="30-39", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="40-49",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="40-49", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="40-49", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="50-59",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="50-59", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="50-59", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="60+",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="60+", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="60+", 10])))
                                                       ,0), "%") 


# female percentages of pain for each age category
tdlab.female=tibble(tdlab.female, pcpain="NA")

tdlab.female[tdlab.female$agecat=="10-19",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="10-19", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="10-19", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="20-29",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="20-29", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="20-29", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="30-39",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="30-39", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="30-39", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="40-49",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="40-49", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="40-49", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="50-59",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="50-59", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="50-59", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="60+",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="60+", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="60+", 10])))
                                                           ,0), "%") 
## --------------------- ##

## 2) compute number of person-observations for each age category. ##
#male
tdlab.male[tdlab.male$agecat=="10-19",16]<-paste0("10-19 \n(N=",dim(tdlab.male[tdlab.male$agecat=="10-19",])[1], ")")
tdlab.male[tdlab.male$agecat=="20-29",16]<-paste0("20-29 \n(N=",dim(tdlab.male[tdlab.male$agecat=="20-29",])[1], ")")
tdlab.male[tdlab.male$agecat=="30-39",16]<-paste0("30-39 \n(N=",dim(tdlab.male[tdlab.male$agecat=="30-39",])[1], ")")
tdlab.male[tdlab.male$agecat=="40-49",16]<-paste0("40-49 \n(N=",dim(tdlab.male[tdlab.male$agecat=="40-49",])[1], ")")
tdlab.male[tdlab.male$agecat=="50-59",16]<-paste0("50-59 \n(N=",dim(tdlab.male[tdlab.male$agecat=="50-59",])[1], ")")
tdlab.male[tdlab.male$agecat=="60+",16]<-paste0("60+ \n(N=",dim(tdlab.male[tdlab.male$agecat=="60+",])[1], ")")

#female
tdlab.female[tdlab.female$agecat=="10-19",16]<-paste0("10-19 \n(N=",dim(tdlab.female[tdlab.female$agecat=="10-19",])[1], ")")
tdlab.female[tdlab.female$agecat=="20-29",16]<-paste0("20-29 \n(N=",dim(tdlab.female[tdlab.female$agecat=="20-29",])[1], ")")
tdlab.female[tdlab.female$agecat=="30-39",16]<-paste0("30-39 \n(N=",dim(tdlab.female[tdlab.female$agecat=="30-39",])[1], ")")
tdlab.female[tdlab.female$agecat=="40-49",16]<-paste0("40-49 \n(N=",dim(tdlab.female[tdlab.female$agecat=="40-49",])[1], ")")
tdlab.female[tdlab.female$agecat=="50-59",16]<-paste0("50-59 \n(N=",dim(tdlab.female[tdlab.female$agecat=="50-59",])[1], ")")
tdlab.female[tdlab.female$agecat=="60+",16]<-paste0("60+ \n(N=",dim(tdlab.female[tdlab.female$agecat=="60+",])[1], ")")
dlabdata=rbind(tdlab.female,tdlab.male)
dlabdata$agecat<-factor(dlabdata$agecat, levels=c("10-19 \n(N=41)", "20-29 \n(N=97)" ,"30-39 \n(N=63)" ,"40-49 \n(N=91)","50-59 \n(N=26)", "60+ \n(N=17)","10-19 \n(N=16)", "20-29 \n(N=127)","30-39 \n(N=91)" , "40-49 \n(N=72)" ,"50-59 \n(N=54)" , "60+ \n(N=34)"))
## --------------------- ##

## 3) plotting the figure ##

library(ggplot2)
library(ggsci)

#find colorblind friendly palette
palette.colors(palette = "Okabe-Ito")
# specifying the colours used for each pain category
colfill <- c("Fall / other accident" = "#009E73",  "Work" = "#0072B2", "Reproduction" = "#999999", "Weather" = "#D55E00", "Illness" = "#E69F00", "Social" = "#56B4E9", "Old age"="#000000")

#specifying the text for gender
gender_names <- c(`0` = "Female",`1` = "Male")

fig1 <- ggplot(dlabdata, aes(x = agecat)) +
  facet_wrap(~ male, labeller = as_labeller(gender_names), scales = "free_x")+
  theme_bw()+
  geom_bar(aes(fill = cause.rev), position = "fill", width=0.6) +
  geom_text(aes(y= 1.025,label = pcpain),
            position = position_dodge(width = 1),
            vjust = -0.5, size = 10
  )+
  theme(legend.position = "top")+
  labs(fill="")+
  ylab("Percent of pain attributions") +
  xlab("Age category") +
  theme(text = element_text(size=55),plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_y_continuous(breaks = seq(0, 1, .2), limits=c(0,1.1), labels = scales::percent) +
  scale_fill_manual(values = colfill)


fig1
dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/Figure1.tiff", width=2000, height=1000, units="px")
dev.off()
######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE S2 ######### 

data.male=data[data$male=="1",]
data.female=data[data$male=="0",]

#### A) building 3 datasets, one for each of the 3 panels presented in Figure S2 : 1) acute pain, 2) chronic pain >= 90 days, 3) chronic pain >= 180 days ####
### 1) acute pain (Pain < 90 days) ###

tdata=data[data$Chronic.Pain90=="0",]
ttdata.male=tdata[tdata$male=="1",]
ttdata.female=tdata[tdata$male=="0",]
tdata.male=ttdata.male[!is.na(ttdata.male$pid),]
tdata.female=ttdata.female[!is.na(ttdata.female$pid),]

dataacute<-ddata[ddata$Chronic.Pain90==0,]

## a) Percentage of pain by age category and gender ##
tdlab.male=dataacute[dataacute$male=='1',]
tdlab.female=dataacute[dataacute$male=='0',]

# male
tdlab.male=tibble(tdlab.male,pcpain="NA")# will be used to store male percentages of pain for each age category

# computing percentage of individuals having pain: all the PIDs that have acute pain for the same age category and sex, divided by all the PIDs of the individuals of the same age category and sex
tdlab.male[tdlab.male$agecat=="10-19",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="10-19", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="10-19", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="20-29",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="20-29", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="20-29", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="30-39",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="30-39", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="30-39", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="40-49",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="40-49", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="40-49", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="50-59",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="50-59", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="50-59", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="60+",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="60+", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="60+", 10])))
                                                       ,0), "%") 


# female
tdlab.female=tibble(tdlab.female,pcpain="NA")

tdlab.female[tdlab.female$agecat=="10-19",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="10-19", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="10-19", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="20-29",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="20-29", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="20-29", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="30-39",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="30-39", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="30-39", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="40-49",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="40-49", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="40-49", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="50-59",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="50-59", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="50-59", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="60+",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="60+", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="60+", 10])))
                                                           ,0), "%") 
## --------------------- ##

## b) compute number of person-observations for each age category. ##
# male
tdlab.male[tdlab.male$agecat=="10-19",16]<-paste0("10-19 \n(N=",dim(tdlab.male[tdlab.male$agecat=="10-19",])[1], ")")
tdlab.male[tdlab.male$agecat=="20-29",16]<-paste0("20-29 \n(N=",dim(tdlab.male[tdlab.male$agecat=="20-29",])[1], ")")
tdlab.male[tdlab.male$agecat=="30-39",16]<-paste0("30-39 \n(N=",dim(tdlab.male[tdlab.male$agecat=="30-39",])[1], ")")
tdlab.male[tdlab.male$agecat=="40-49",16]<-paste0("40-49 \n(N=",dim(tdlab.male[tdlab.male$agecat=="40-49",])[1], ")")
tdlab.male[tdlab.male$agecat=="50-59",16]<-paste0("50-59 \n(N=",dim(tdlab.male[tdlab.male$agecat=="50-59",])[1], ")")
tdlab.male[tdlab.male$agecat=="60+",16]<-paste0("60+ \n(N=",dim(tdlab.male[tdlab.male$agecat=="60+",])[1], ")")

# female
tdlab.female[tdlab.female$agecat=="10-19",16]<-paste0("10-19 \n(N=",dim(tdlab.female[tdlab.female$agecat=="10-19",])[1], ")")
tdlab.female[tdlab.female$agecat=="20-29",16]<-paste0("20-29 \n(N=",dim(tdlab.female[tdlab.female$agecat=="20-29",])[1], ")")
tdlab.female[tdlab.female$agecat=="30-39",16]<-paste0("30-39 \n(N=",dim(tdlab.female[tdlab.female$agecat=="30-39",])[1], ")")
tdlab.female[tdlab.female$agecat=="40-49",16]<-paste0("40-49 \n(N=",dim(tdlab.female[tdlab.female$agecat=="40-49",])[1], ")")
tdlab.female[tdlab.female$agecat=="50-59",16]<-paste0("50-59 \n(N=",dim(tdlab.female[tdlab.female$agecat=="50-59",])[1], ")")
tdlab.female[tdlab.female$agecat=="60+",16]<-paste0("60+ \n(N=",dim(tdlab.female[tdlab.female$agecat=="60+",])[1], ")")

# store male and female counts of person-observations
dlabdatapacute=rbind(tdlab.female,tdlab.male)
pacutelevels=c("10-19 \n(N=35)", "20-29 \n(N=58)" , "30-39 \n(N=41)", "40-49 \n(N=52)" ,"50-59 \n(N=13)" , "60+ \n(N=6)" , "10-19 \n(N=15)" ,  "20-29 \n(N=99)", "30-39 \n(N=64)", "40-49 \n(N=41)", "50-59 \n(N=25)", "60+ \n(N=10)")
dlabdatapacute$agecat<-factor(dlabdatapacute$agecat, levels=pacutelevels)
### ---------------------------------------------------------------------------------------------------------------------- ###

### 2) chronic pain (Pain >= 90 days) ###
# data and tdata must only have data with Chronic.Pain90==1
data90<-ddata[ddata$Chronic.Pain90==1,]
tdlabdata<-arrange(data90, pid)
data.male=data[data$male=="1",]
data.female=data[data$male=="0",]

tdata=data[data$Chronic.Pain90=="1",]
ttdata.male=tdata[tdata$male=="1",]
ttdata.female=tdata[tdata$male=="0",]
tdata.male=ttdata.male[!is.na(ttdata.male$pid),]
tdata.female=ttdata.female[!is.na(ttdata.female$pid),]

## a) Percentage of pain by age category and gender ##
tdlab.male=data90[data90$male=='1',]
tdlab.female=data90[data90$male=='0',]

tdlab.male=tibble(tdlab.male,pcpain="NA")
# computing percentage of individuals having pain: all the PIDs that have chronic pain (>=90 days) for the same age category and sex, divided by all the PIDs of the individuals of the same age category and sex 
tdlab.male[tdlab.male$agecat=="10-19",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="10-19", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="10-19", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="20-29",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="20-29", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="20-29", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="30-39",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="30-39", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="30-39", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="40-49",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="40-49", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="40-49", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="50-59",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="50-59", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="50-59", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="60+",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="60+", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="60+", 10])))
                                                       ,0), "%") 

# female
tdlab.female=tibble(tdlab.female,pcpain="NA")
# computing percentage of individuals having pain: all the PIDs that have chronic pain (>=90 days) for the same age category and sex, divided by all the PIDs of the individuals of the same age category and sex 
tdlab.female[tdlab.female$agecat=="10-19",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="10-19", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="10-19", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="20-29",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="20-29", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="20-29", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="30-39",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="30-39", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="30-39", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="40-49",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="40-49", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="40-49", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="50-59",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="50-59", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="50-59", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="60+",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="60+", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="60+", 10])))
                                                           ,0), "%") 
## --------------------- ##

## b) compute number of person-observations for each age category. ##
# male
tdlab.male[tdlab.male$agecat=="10-19",16]<-paste0("10-19 \n(N=",dim(tdlab.male[tdlab.male$agecat=="10-19",])[1], ")")
tdlab.male[tdlab.male$agecat=="20-29",16]<-paste0("20-29 \n(N=",dim(tdlab.male[tdlab.male$agecat=="20-29",])[1], ")")
tdlab.male[tdlab.male$agecat=="30-39",16]<-paste0("30-39 \n(N=",dim(tdlab.male[tdlab.male$agecat=="30-39",])[1], ")")
tdlab.male[tdlab.male$agecat=="40-49",16]<-paste0("40-49 \n(N=",dim(tdlab.male[tdlab.male$agecat=="40-49",])[1], ")")
tdlab.male[tdlab.male$agecat=="50-59",16]<-paste0("50-59 \n(N=",dim(tdlab.male[tdlab.male$agecat=="50-59",])[1], ")")
tdlab.male[tdlab.male$agecat=="60+",16]<-paste0("60+ \n(N=",dim(tdlab.male[tdlab.male$agecat=="60+",])[1], ")")

# female
tdlab.female[tdlab.female$agecat=="10-19",16]<-paste0("10-19 \n(N=",dim(tdlab.female[tdlab.female$agecat=="10-19",])[1], ")")
tdlab.female[tdlab.female$agecat=="20-29",16]<-paste0("20-29 \n(N=",dim(tdlab.female[tdlab.female$agecat=="20-29",])[1], ")")
tdlab.female[tdlab.female$agecat=="30-39",16]<-paste0("30-39 \n(N=",dim(tdlab.female[tdlab.female$agecat=="30-39",])[1], ")")
tdlab.female[tdlab.female$agecat=="40-49",16]<-paste0("40-49 \n(N=",dim(tdlab.female[tdlab.female$agecat=="40-49",])[1], ")")
tdlab.female[tdlab.female$agecat=="50-59",16]<-paste0("50-59 \n(N=",dim(tdlab.female[tdlab.female$agecat=="50-59",])[1], ")")
tdlab.female[tdlab.female$agecat=="60+",16]<-paste0("60+ \n(N=",dim(tdlab.female[tdlab.female$agecat=="60+",])[1], ")")

# store male and female counts of person- observations
dlabdatap90=rbind(tdlab.female,tdlab.male)
p90levels=c("10-19 \n(N=1)", "20-29 \n(N=21)" , "30-39 \n(N=18)", "40-49 \n(N=25)" ,"50-59 \n(N=21)" , "60+ \n(N=15)" , "10-19 \n(N=5)" ,  "20-29 \n(N=31)", "30-39 \n(N=13)", "40-49 \n(N=34)", "50-59 \n(N=11)", "60+ \n(N=6)")
dlabdatap90$agecat<-factor(dlabdatap90$agecat, levels=p90levels)
### ---------------------------------------------------------------------------------------------------------------------- ###

### 3) chronic pain (Pain >= 180 days) ###
# data and tdata must only have data with Chronic.Pain180==1
data180<-ddata[ddata$Chronic.Pain180==1,]

tdlab.male=data180[data180$male=='1',]
tdlab.female=data180[data180$male=='0',]
tdlab.male=tibble(tdlab.male,pcpain="NA")

tdata=data[data$Chronic.Pain180=="1",]
ttdata.male=tdata[tdata$male=="1",]
ttdata.female=tdata[tdata$male=="0",]
tdata.male=ttdata.male[!is.na(ttdata.male$pid),]
tdata.female=ttdata.female[!is.na(ttdata.female$pid),]

## a) Percentage of pain by age category and gender ##
tdlab.male=data180[data180$male=='1',]
tdlab.female=data180[data180$male=='0',]

# male
tdlab.male=tibble(tdlab.male,pcpain="NA")
# computing percentage of individuals having pain: all the PIDs that have chronic pain (>=180 days) for the same age category and sex, divided by all the PIDs of the individuals of the same age category and sex 
tdlab.male[tdlab.male$agecat=="10-19",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="10-19", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="10-19", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="20-29",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="20-29", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="20-29", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="30-39",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="30-39", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="30-39", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="40-49",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="40-49", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="40-49", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="50-59",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="50-59", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="50-59", 10])))
                                                         ,0), "%") 
tdlab.male[tdlab.male$agecat=="60+",75]<- paste0(round(length(unique(unlist(tdata.male[tdata.male$agecat=="60+", 10])))*100/length(unique(unlist(data.male[data.male$agecat=="60+", 10])))
                                                       ,0), "%") 

# female
tdlab.female=tibble(tdlab.female,pcpain="NA")

tdlab.female[tdlab.female$agecat=="10-19",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="10-19", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="10-19", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="20-29",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="20-29", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="20-29", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="30-39",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="30-39", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="30-39", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="40-49",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="40-49", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="40-49", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="50-59",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="50-59", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="50-59", 10])))
                                                             ,0), "%") 
tdlab.female[tdlab.female$agecat=="60+",75]<- paste0(round(length(unique(unlist(tdata.female[tdata.female$agecat=="60+", 10])))*100/length(unique(unlist(data.female[data.female$agecat=="60+", 10])))
                                                           ,0), "%") 
## --------------------- ##

## b) compute number of person-observations for each age category. ##
# male
tdlab.male[tdlab.male$agecat=="10-19",16]<-paste0("10-19 \n(N=",dim(tdlab.male[tdlab.male$agecat=="10-19",])[1], ")")
tdlab.male[tdlab.male$agecat=="20-29",16]<-paste0("20-29 \n(N=",dim(tdlab.male[tdlab.male$agecat=="20-29",])[1], ")")
tdlab.male[tdlab.male$agecat=="30-39",16]<-paste0("30-39 \n(N=",dim(tdlab.male[tdlab.male$agecat=="30-39",])[1], ")")
tdlab.male[tdlab.male$agecat=="40-49",16]<-paste0("40-49 \n(N=",dim(tdlab.male[tdlab.male$agecat=="40-49",])[1], ")")
tdlab.male[tdlab.male$agecat=="50-59",16]<-paste0("50-59 \n(N=",dim(tdlab.male[tdlab.male$agecat=="50-59",])[1], ")")
tdlab.male[tdlab.male$agecat=="60+",16]<-paste0("60+ \n(N=",dim(tdlab.male[tdlab.male$agecat=="60+",])[1], ")")

# female
tdlab.female[tdlab.female$agecat=="10-19",16]<-paste0("10-19 \n(N=",dim(tdlab.female[tdlab.female$agecat=="10-19",])[1], ")")
tdlab.female[tdlab.female$agecat=="20-29",16]<-paste0("20-29 \n(N=",dim(tdlab.female[tdlab.female$agecat=="20-29",])[1], ")")
tdlab.female[tdlab.female$agecat=="30-39",16]<-paste0("30-39 \n(N=",dim(tdlab.female[tdlab.female$agecat=="30-39",])[1], ")")
tdlab.female[tdlab.female$agecat=="40-49",16]<-paste0("40-49 \n(N=",dim(tdlab.female[tdlab.female$agecat=="40-49",])[1], ")")
tdlab.female[tdlab.female$agecat=="50-59",16]<-paste0("50-59 \n(N=",dim(tdlab.female[tdlab.female$agecat=="50-59",])[1], ")")
tdlab.female[tdlab.female$agecat=="60+",16]<-paste0("60+ \n(N=",dim(tdlab.female[tdlab.female$agecat=="60+",])[1], ")")

# one age category has no person-observations -> addition of a dummy row for the plot 
dummy=tdlab.male[1,]
dummy$pid<-"DUMMY"
dummy$agecat<-"10-19 \n(N=0)"
dummy$pcpain=" "
dummy$cause.rev="DUMMY"
dlabdatap180=rbind(tdlab.female,tdlab.male,dummy)
p180levels=c("10-19 \n(N=0)", "20-29 \n(N=11)",  "30-39 \n(N=13)",  "40-49 \n(N=18)","50-59 \n(N=12)",  "60+ \n(N=10)",  "10-19 \n(N=1)" , "20-29 \n(N=21)", "30-39 \n(N=10)", "40-49 \n(N=28)", "50-59 \n(N=9)", "60+ \n(N=5)")
dlabdatap180$agecat<-factor(dlabdatap180$agecat, levels=p180levels)
#### --------------------------------------------------------------------------------------------------------------------------------------------------- ####

#### B) based on the 3 previously built datasets, coding of the 3 panels presented in Figure S2 : 1) acute pain, 2) chronic pain >= 90 days, 3) chronic pain >= 180days ####

### 1) acute pain ###
s2.acute <- ggplot(dlabdatapacute, aes(x = agecat)) +
  facet_wrap(~ male, labeller = as_labeller(gender_names), scales = "free_x")+
  theme_bw()+
  geom_bar(aes(fill = cause.rev), position = "fill", width=0.5) +
  geom_text(aes(y= 1.025,label = pcpain),
            position = position_dodge(width = 1),
            vjust = -0.5, size = 10
  )+
  labs(title="Pain < 3 months", fill="")+
  ylab("") +
  xlab("") +
  theme(text = element_text(size=55),plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+
  scale_fill_manual(values = colfill)+
  scale_y_continuous(breaks = seq(0, 1, .2), limits=c(0,1.1), labels = scales::percent)
### ----------- ###

### 2) chronic pain >= 90 days ###
s2.90 <- ggplot(dlabdatap90, aes(x = agecat)) +
  facet_wrap(~ male, labeller = as_labeller(gender_names), scales = "free_x")+
  theme_bw()+
  geom_bar(aes(fill = cause.rev), position = "fill", width=0.5) +
  geom_text(aes(y= 1.025,label = pcpain),
            position = position_dodge(width = 1),
            vjust = -0.5, size = 10
  )+
  labs(title=expression("Pain">="3 months"), fill="")+
  ylab("Percent of pain attributions") +
  xlab("") +
  theme(text = element_text(size=55),plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="none")+
  scale_fill_manual(values = colfill)+
  scale_y_continuous(breaks = seq(0, 1, .2), limits=c(0,1.1), labels = scales::percent)
### ----------- ###

### 3) chronic pain >= 180days ###
# specifying colours for each category of work
colfill.180 <- c("Fall / other accident" = "#009E73",  "Work" = "#0072B2", "Reproduction" = "#999999", "Weather" = "#D55E00", "Illness" = "#E69F00", "Social" = "#56B4E9", "Old age"="#000000", "DUMMY"="white")

s2.180 <- ggplot(dlabdatap180, aes(x = agecat)) +
  facet_wrap(~ male, labeller = as_labeller(gender_names), scales = "free_x")+
  theme_bw()+
  geom_bar(aes(fill = cause.rev), position = "fill", width=0.5) +
  geom_text(aes(y= 1.025,label = pcpain),
            position = position_dodge(width = 1),
            vjust = -0.5, size = 10
  )+
  labs(title=expression("Pain">="6 months"), fill="")+
  ylab("") +
  xlab("Age category") +
  theme(text = element_text(size=55),plot.title = element_text(hjust = 0.5),  panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position="bottom")+
  scale_fill_manual(values = colfill.180)+
  scale_y_continuous(breaks = seq(0, 1, .2), limits=c(0,1.1), labels = scales::percent)
### ----------- ###


### 4) merging the 3 subplots into one figure ###
library(ggplot2); library(ggpubr)
ggarrange(s2.acute, s2.90, s2.180, nrow=3,ncol=1, label.x="Age category", label.y="Percent of pain attributions",
          common.legend = TRUE, legend = "bottom")

dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/figureS2.tiff", width=2000, height=2000, units="px")
dev.off()
######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S2 ######### 
## N ##
ncause.rev=c(dim(ddata[ddata$cause.rev=="Work",])[1], dim(ddata[ddata$cause.rev=="Fall / other accident",])[1], dim(ddata[ddata$cause.rev=="Social",])[1], dim(ddata[ddata$cause.rev=="Weather",])[1], dim(ddata[ddata$cause.rev=="Illness",])[1], dim(ddata[ddata$cause.rev=="Reproduction",])[1],  dim(ddata[ddata$cause.rev=="Old age",])[1] )
## --------------------- ##

nonaPDdata=ddata[!is.na(ddata$Pain.Days),]

## Mean ##
mean.paindays=c(mean(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Work",62]))), mean(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Fall / other accident",62]))), mean(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Social",62]))), mean(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Weather",62]))), mean(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Illness",62]))), mean(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Reproduction",62]))), mean(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Old age",62]))))
## --------------------- ##

## Median ##
median.paindays=c(median(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Work",62]))), median(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Fall / other accident",62]))), median(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Social",62]))), median(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Weather",62]))), median(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Illness",62]))), median(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Reproduction",62]))), median(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Old age",62]))))
## --------------------- ##

## SD ##
sd.paindays=c(sd(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Work",62]))), sd(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Fall / other accident",62]))), sd(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Social",62]))), sd(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Weather",62]))), sd(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Illness",62]))), sd(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Reproduction",62]))), sd(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Old age",62]))))
## --------------------- ##

## MIN ##
min.paindays=c(min(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Work",62]))), min(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Fall / other accident",62]))), min(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Social",62]))), min(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Weather",62]))), min(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Illness",62]))), min(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Reproduction",62]))), min(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Old age",62]))))
## --------------------- ##

## MAX ##
max.paindays=c(max(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Work",62]))), max(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Fall / other accident",62]))), max(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Social",62]))), max(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Weather",62]))), max(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Illness",62]))), max(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Reproduction",62]))), max(as.numeric(unlist(nonaPDdata[nonaPDdata$cause.rev=="Old age",62]))))
## --------------------- ##

library(tibble)
table.s2=tibble(`Self-reported cause of pain`=c("Work", "Fall / other accident", "Social","Weather", "Illness", "Reproduction", "Old age"), N = ncause.rev, Mean = mean.paindays, Median = median.paindays, SD = sd.paindays, Min = min.paindays, Max= max.paindays)
table.s2
format(table.s2, digits=2, nsmall=2)

write.csv(table.s2, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS2.csv")
######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE S3 ######### 
# changing typo of categories of work-related pain
data[data$cause.work=="overload",65]<-"Overload"
data[data$cause.work=="domestic",65]<-"Domestic"
data[data$cause.work=="fish",65]<-"Fish"
data[data$cause.work=="horticulture",65]<-"Horticulture"
data[data$cause.work=="hunt or forage",65]<-"Hunt or forage"
data[data$cause.work=="nonspecific",65]<-"Nonspecific"
data[data$cause.work=="transport",65]<-"Transport"
data[data$cause.work=="wood",65]<-"Wood"

dddata=data[!data$cause.work=="NA",]
dddata=dddata[!dddata$cause.work=="Nonspecific",] # excluding rows that have "Nonspecific" as a work-related cause of pain
dddata$cause.work<-factor(dddata$cause.work, levels = c("Horticulture", "Overload", "Hunt or forage", "Transport", "Wood", "Fish", "Domestic")) # specifying the order of priority for the categories of cause.work for later plotting

# divide the dataset in male and female subsets
tdlab.male=dddata[dddata$male=='1',]
tdlab.female=dddata[dddata$male=='0',]

# storing the correct order of each age category text in order to display it on the figure 
vec.male=c(paste0("10-19 \n(N=",dim(tdlab.male[tdlab.male$agecat=="10-19",])[1], ")"), paste0("20-29 \n(N=",dim(tdlab.male[tdlab.male$agecat=="20-29",])[1], ")"),paste0("30-39 \n(N=",dim(tdlab.male[tdlab.male$agecat=="30-39",])[1], ")")
           ,paste0("40-49 \n(N=",dim(tdlab.male[tdlab.male$agecat=="40-49",])[1], ")"),paste0("50-59 \n(N=",dim(tdlab.male[tdlab.male$agecat=="50-59",])[1], ")"), paste0("60+ \n(N=",dim(tdlab.male[tdlab.male$agecat=="60+",])[1], ")"))
vec.female=c(paste0("10-19 \n(N=",dim(tdlab.female[tdlab.female$agecat=="10-19",])[1], ")"), paste0("20-29 \n(N=",dim(tdlab.female[tdlab.female$agecat=="20-29",])[1], ")"),paste0("30-39 \n(N=",dim(tdlab.female[tdlab.female$agecat=="30-39",])[1], ")")
             ,paste0("40-49 \n(N=",dim(tdlab.female[tdlab.female$agecat=="40-49",])[1], ")"),paste0("50-59 \n(N=",dim(tdlab.female[tdlab.female$agecat=="50-59",])[1], ")"), paste0("60+ \n(N=",dim(tdlab.female[tdlab.female$agecat=="60+",])[1], ")"))

## 1) compute number of person-observations for each age category ##
# male
tdlab.male[tdlab.male$agecat=="10-19",16]<-paste0("10-19 \n(N=",dim(tdlab.male[tdlab.male$agecat=="10-19",])[1], ")")
tdlab.male[tdlab.male$agecat=="20-29",16]<-paste0("20-29 \n(N=",dim(tdlab.male[tdlab.male$agecat=="20-29",])[1], ")")
tdlab.male[tdlab.male$agecat=="30-39",16]<-paste0("30-39 \n(N=",dim(tdlab.male[tdlab.male$agecat=="30-39",])[1], ")")
tdlab.male[tdlab.male$agecat=="40-49",16]<-paste0("40-49 \n(N=",dim(tdlab.male[tdlab.male$agecat=="40-49",])[1], ")")
tdlab.male[tdlab.male$agecat=="50-59",16]<-paste0("50-59 \n(N=",dim(tdlab.male[tdlab.male$agecat=="50-59",])[1], ")")
tdlab.male[tdlab.male$agecat=="60+",16]<-paste0("60+ \n(N=",dim(tdlab.male[tdlab.male$agecat=="60+",])[1], ")")

# female
tdlab.female[tdlab.female$agecat=="10-19",16]<-paste0("10-19 \n(N=",dim(tdlab.female[tdlab.female$agecat=="10-19",])[1], ")")
tdlab.female[tdlab.female$agecat=="20-29",16]<-paste0("20-29 \n(N=",dim(tdlab.female[tdlab.female$agecat=="20-29",])[1], ")")
tdlab.female[tdlab.female$agecat=="30-39",16]<-paste0("30-39 \n(N=",dim(tdlab.female[tdlab.female$agecat=="30-39",])[1], ")")
tdlab.female[tdlab.female$agecat=="40-49",16]<-paste0("40-49 \n(N=",dim(tdlab.female[tdlab.female$agecat=="40-49",])[1], ")")
tdlab.female[tdlab.female$agecat=="50-59",16]<-paste0("50-59 \n(N=",dim(tdlab.female[tdlab.female$agecat=="50-59",])[1], ")")
tdlab.female[tdlab.female$agecat=="60+",16]<-paste0("60+ \n(N=",dim(tdlab.female[tdlab.female$agecat=="60+",])[1], ")")

# fusing male and female counts of person-observations per age category
dlabdata=rbind(tdlab.female,tdlab.male)
dlabdata$agecat<-factor(dlabdata$agecat, levels=c(vec.male,vec.female))
## ----------- ##

## 2) coding of the plot ##
#find colorblind friendly palette
palette.colors(palette = "Alphabet")
# specifying colours for each category of work
colfill2 <- c("Overload" = "#325A9B",  "Hunt or forage" = "#F6222E", "Horticulture" = "#1C8356", "Transport" = "#B00068", "Fish" = "#F7E1A0", "Domestic" = "#FEAF16", "Wood"="#1CBE4F")

fig.S3 <- ggplot(dlabdata, aes(x = agecat)) +
  facet_wrap(~ male, labeller = as_labeller(gender_names), scales = "free_x")+
  theme_bw()+
  geom_bar(aes(fill = cause.work), position = "fill",  width=0.6) +
  theme(legend.position = "top")+
  labs(fill="")+
  ylab("Percent of work-related pain attributions") +
  xlab("Age category") +
  theme(text = element_text(size=55),plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values = colfill2)+
  scale_y_continuous(labels = scales::percent)


fig.S3
dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/figureS3.tiff", width=2000, height=1000, units="px")
dev.off()
######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S3 ######### 
## N ##
ncause.work=c(dim(dddata[dddata$cause.work=="Horticulture",])[1], dim(dddata[dddata$cause.work=="Overload",])[1], dim(dddata[dddata$cause.work=="Transport",])[1], dim(dddata[dddata$cause.work=="Hunt or forage",])[1], dim(dddata[dddata$cause.work=="Fish",])[1], dim(dddata[dddata$cause.work=="Wood",])[1],  dim(dddata[dddata$cause.work=="Domestic",])[1] )
## ----------- ##

nonaPdddata=dddata[!dddata$Pain.Days=="NA",]
nonaPdddata=nonaPdddata[!is.na(nonaPdddata$Pain.Days),]

## Mean ##
mean.paindays=c(mean(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Horticulture",62]))), mean(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Overload",62]))), mean(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Transport",62]))), mean(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Hunt or forage",62]))), mean(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Fish",62]))), mean(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Wood",62]))), mean(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Domestic",62]))))
## ----------- ##

## Median ##
median.paindays=c(median(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Horticulture",62]))), median(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Overload",62]))), median(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Transport",62]))), median(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Hunt or forage",62]))), median(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Fish",62]))), median(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Wood",62]))), median(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Domestic",62]))))
## ----------- ##

## SD ##
sd.paindays=c(sd(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Horticulture",62]))), sd(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Overload",62]))), sd(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Transport",62]))), sd(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Hunt or forage",62]))), sd(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Fish",62]))), sd(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Wood",62]))), sd(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Domestic",62]))))
## ----------- ##

## Min ##
min.paindays=c(min(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Horticulture",62]))), min(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Overload",62]))), min(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Transport",62]))), min(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Hunt or forage",62]))), min(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Fish",62]))), min(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Wood",62]))), min(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Domestic",62]))))
## ----------- ##

## Max ##
max.paindays=c(max(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Horticulture",62]))), max(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Overload",62]))), max(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Transport",62]))), max(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Hunt or forage",62]))), max(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Fish",62]))), max(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Wood",62]))), max(as.numeric(unlist(nonaPdddata[nonaPdddata$cause.work=="Domestic",62]))))
## ----------- ##

library(tibble)
table.s3=tibble(`Work-related pain attribution`=c("Horticulture", "Overload", "Transport (e.g. walking)","Hunt or forage", "Fish", "Wood (i.e. logging)", "Domestic"), N = ncause.work, Mean = mean.paindays, Median = median.paindays, SD = sd.paindays, Min = min.paindays, Max= max.paindays)
table.s3
format(table.s3, digits=2, nsmall=2)

write.csv(table.s3, "/home/yoann/Bureau//PAIN TSIMANE REVIEWER VERSION/TABLES/tableS3.csv")
######### --------------------------------------------------------------------------------------------------- #########


######### TABLE S4 ######### 
library(glmmTMB)

# coding arm as the reference 
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("arm", "back","foot", "hand", "leg"))

model4 <- glmmTMB(Pain ~ z_age + male + Anatomical.location + (1|pid) + (1|comID), data=data, family=binomial)

## Estimate (log odds) ##
est.4=round(summary(model4)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.4=round(exp(summary(model4)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.4=round(summary(model4)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.4=round(summary(model4)$coefficients$cond[,4],2)
tpvalue=pv.4
pvalue=pv.4

# loop to display correctly p-value (not raw values)
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.4=pvalue
## ----------- ##

## Logit ## 
log.4=summary(model4)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.4=summary(model4)$nobs
nobs.npid.ncomID=c(nobs.4,summary(model4)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.4=c(round(summary(model4)$varcor$cond$pid[1],2), round(summary(model4)$varcor$cond$comID[1],2))
names(randvar.4)<-c("pid", "comID")
## ----------- ##

list4=list(est.4, or.4, sd.4, pv.4, log.4, nobs.npid.ncomID, randvar.4)
names(list4)<-c("Estimate (log odds)", "Odds ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects")

capture.output(list4, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS4.csv")

# back to reference order of anatomical location
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("back","foot", "hand", "leg", "arm"))

######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE 2 ######### 

## 1) generate predicted values for all possible levels and values from model ##
library(glmmTMB)
model.2 <- glmmTMB(Pain ~ z_age + male + Anatomical.location + (1|pid) + (1|comID), data=data, family=binomial)

library(ggeffects)
grid=ggpredict(model.2, terms =c("z_age [all]","male" , "Anatomical.location" )) # storing the predicted values of pain for each value of z-scaled age  
## ----------- ##

## 2) coding of the plot ##
# rename the facets legend in the facet_grid()
analoc.names <- c( `foot` = "Foot",`leg` = "Leg", `back` = "Lower back",`hand` = "Hand", `arm` = "Arm")
gender_names <- c(`0` = "Female",`1` = "Male")

# specify the captions "breaks" to use in the plot as x axis labels, and the corresponding z-scaled x values stored in "scaledbreaks"
breaks=c(NA,20,40,60, NA)
scaledbreaks=(breaks-mean(data$age))/sd(data$age)

library(ggplot2)
fig2<-ggplot(grid, aes(x)) +
  geom_line(aes(y=predicted, colour = factor(group, labels=gender_names)))+
  facet_grid(~facet, labeller = as_labeller(analoc.names))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(group, labels=gender_names)), alpha = 0.2)+
  xlab("Age (years)") +
  ylab("Predicted probability of pain") +
  theme_bw()+
  theme(text = element_text(size=42),plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "top" )+
  scale_x_continuous(breaks = scaledbreaks, labels = breaks, limits=c(min(scaledbreaks), max(scaledbreaks)))+  # use the values in "scaledbreaks" as x axis values, but labelled as the values stored in "breaks"
  scale_y_continuous(labels = scales::percent)+
  labs(fill= " ", colour= " ")


fig2

dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/Figure2.tiff", width=2000, height=1000, units="px")
dev.off()
######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S5 #########
# coding arm as the reference 
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("arm", "back","foot", "hand", "leg"))

duration=data[!is.na(data$Pain.Days),]

model5 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location + (1|pid) + (1|comID), data=duration, family=genpois, ziformula= ~ 1)

## Estimate (log odds) ##
est.5=round(summary(model5)$coefficients$cond[,1],3)
## ----------- ## 

## Incident rate ratio ##
or.5=round(exp(summary(model5)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.5=round(summary(model5)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.5=round(summary(model5)$coefficients$cond[,4],2)
tpvalue=pv.5
pvalue=pv.5
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.5=pvalue
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
log.5=summary(model5)$family
nobs.5=summary(model5)$nobs
nobs.npid.ncomID=c(nobs.5,summary(model5)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.5=c(round(summary(model5)$varcor$cond$pid[1],2), round(summary(model5)$varcor$cond$comID[1],2))
names(randvar.5)<-c("pid", "comID")
## ----------- ##


## Zero inflation model ##
## Estimate (log odds) ##
est.zim.5=round(summary(model5)$coefficients$zi[,1],3)
## ----------- ##

## Odds ratio ##
or.zim.5=round(exp(summary(model5)$coefficients$zi[,1]),3)          
## ----------- ##

## SD ##
sd.zim.5=round(summary(model5)$coefficients$zi[,2],2)
## ----------- ##

## P-values ##
pv.zim.5=round(summary(model5)$coefficients$zi[,4],2)
tpvalue=pv.zim.5
pvalue=pv.zim.5
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.zim.5=pvalue
## ----------- ##

list15=list(est.5, or.5, sd.5, pv.5, log.5, nobs.npid.ncomID, randvar.5, est.zim.5, or.zim.5, sd.zim.5, pv.zim.5)
names(list15)<-c("Estimate (log odds)", "Incident rate ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects", "Estimate (log odds)", "Incident rate ratio", "Standard deviations", "P-values")

capture.output(list5, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS5.csv")

# back to reference order of anatomical location
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("back","foot", "hand", "leg", "arm"))

######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE S4 ######### 

## 1) generate predicted values for all possible levels and values from model ##
# using a dataset composed of rows with complete data (no NA value in pain duration)
duration=data[!is.na(data$Pain.Days),]
library(glmmTMB)
model.s4 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location + (1|pid) + (1|comID), data=duration, family=genpois)

library(ggeffects)
grid=ggpredict(model.s4, terms =c("z_age [all]", "male", "Anatomical.location" )) # storing the predicted values of pain duration (days) for each value of z-scaled age  
## ----------- ##

## 2) coding of the plot ##
# rename the facets legend in the facet_grid()
analoc.names <- c( `foot` = "Foot",`leg` = "Leg", `back` = "Lower back",`hand` = "Hand", `arm` = "Arm")
gender_names <- c(`0` = "Female",`1` = "Male")

# specify the captions "breaks" to use in the plot as x axis labels, and the corresponding z-scaled x values stored in "scaledbreaks"
breaks=c(NA,20,40,60, NA)
scaledbreaks=(breaks-mean(data$age))/sd(data$age)

library(ggplot2)
fig.S4<-ggplot(grid, aes(x)) +
  geom_line(aes(y=predicted, colour = factor(group, labels=gender_names)))+ # draw lines of predicted values for each gender
  facet_grid(~facet, labeller = as_labeller(analoc.names))+ # organized by panels of each anatomical location
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(group, labels=gender_names)), alpha = 0.2)+
  xlab("Age (years)") +
  ylab("Predicted duration of pain (days)") +
  theme_bw()+
  theme(text = element_text(size=42),plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "top" )+
  scale_x_continuous(breaks = scaledbreaks, labels = breaks, limits=c(min(scaledbreaks), max(scaledbreaks)))+ # use the values in "scaledbreaks" as x axis values, but labelled as the values stored in "breaks"
  labs(fill= " ", colour= " ")

fig.S4

dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/figureS4.tiff", width=2000, height=1000, units="px")
dev.off()  
######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S6 #########
# coding arm as the reference 
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("arm", "back","foot", "hand", "leg"))

ddata=data
ddata$Chronic.Pain90<-as.numeric(ddata$Chronic.Pain90)
model6 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location + (1|pid) + (1|comID), data=ddata, family=binomial)

## Estimate (log odds) ##
est.6=round(summary(model6)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.6=round(exp(summary(model6)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.6=round(summary(model6)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.6=round(summary(model6)$coefficients$cond[,4],2)
tpvalue=pv.6
pvalue=pv.6
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.6=pvalue
## ----------- ##

## Link function ##
log.6=summary(model6)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.6=summary(model6)$nobs
nobs.npid.ncomID=c(nobs.6,summary(model6)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.6=c(round(summary(model6)$varcor$cond$pid[1],2), round(summary(model6)$varcor$cond$comID[1],2))
names(randvar.6)<-c("pid", "comID")
## ----------- ##

list6=list(est.6, or.6, sd.6, pv.6, log.6, nobs.npid.ncomID, randvar.6)
names(list6)<-c("Estimate (log odds)", "Odds ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects")
capture.output(list6, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS6.csv")

######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S7 ######### 
ddata$Chronic.Pain180<-as.numeric(ddata$Chronic.Pain180)
model7 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location + (1|pid) + (1|comID), data=ddata, family=binomial)

## Estimate (log odds) ##
est.7=round(summary(model7)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.7=round(exp(summary(model7)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.7=round(summary(model7)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.7=round(summary(model7)$coefficients$cond[,4],2)
tpvalue=pv.7
pvalue=pv.7
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.7=pvalue
## ----------- ##

## Link function ##
log.7=summary(model7)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.7=summary(model7)$nobs
nobs.npid.ncomID=c(nobs.7,summary(model7)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.7=c(round(summary(model7)$varcor$cond$pid[1],2), round(summary(model7)$varcor$cond$comID[1],2))
names(randvar.7)<-c("pid", "comID")
## ----------- ##

list7=list(est.7, or.7, sd.7, pv.7, log.7, nobs.npid.ncomID, randvar.7)
names(list7)<-c("Estimate (log odds)", "Odds ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects")
capture.output(list7, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS7.csv")

# back to reference order of anatomical location
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("back","foot", "hand", "leg", "arm"))

######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE S5 ######### 
data$Chronic.Pain90<-as.numeric(data$Chronic.Pain90)

## 1) generate predicted values for all possible levels and values from model ##
library(glmmTMB)
model.s5 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location + (1|pid) + (1|comID), data=data, family=binomial)

library(ggeffects)
grid=ggpredict(model.s5, terms =c("z_age [all]", "male", "Anatomical.location" )) # storing the predicted values of chronic pain for each continuousof z-scaled age 
## ----------- ##

## 2) coding of the plot ##
# rename the facets legend in the facet_grid()
analoc.names <- c( `foot` = "Foot",`leg` = "Leg", `back` = "Lower back",`hand` = "Hand", `arm` = "Arm")
gender_names <- c(`0` = "Female",`1` = "Male")

# specify the captions "breaks" to use in the plot as x axis labels, and the corresponding z-scaled x values stored in "scaledbreaks"
breaks=c(NA,20,40,60, NA)
scaledbreaks=(breaks-mean(data$age))/sd(data$age)

library(ggplot2)
fig.S5<-ggplot(grid, aes(x)) +
  geom_line(aes(y=predicted, colour = factor(group, labels=gender_names)))+
  facet_grid(~facet, labeller = as_labeller(analoc.names))+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = factor(group, labels=gender_names)), alpha = 0.2)+
  xlab("Age (years)") +
  ylab(expression("Predicted probability of chronic pain">="90 days")) +
  theme_bw()+
  theme(text = element_text(size=42),plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "top" )+
  scale_x_continuous(breaks = scaledbreaks, labels = breaks, limits=c(min(scaledbreaks), max(scaledbreaks)))+
  scale_y_continuous(labels = scales::percent)+
  labs(fill= " ", colour= " ")

fig.S5

dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/figureS5.tiff", width=2000, height=1000, units="px")
dev.off()
######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S8 ######### 
# coding arm as the reference 
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("arm", "back","foot", "hand", "leg"))

model8 <- glmmTMB(Pain ~ z_age + male + Anatomical.location + z_sumdiags + (1|pid) + (1|comID), data=data, family=binomial)

## Estimate (log odds) ##
est.8=round(summary(model8)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.8=round(exp(summary(model8)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.8=round(summary(model8)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.8=round(summary(model8)$coefficients$cond[,4],2)
tpvalue=pv.8
pvalue=pv.8
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.8=pvalue
## ----------- ##

## Link function ##
log.8=summary(model8)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.8=summary(model8)$nobs
nobs.npid.ncomID=c(nobs.8,summary(model8)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.8=c(round(summary(model8)$varcor$cond$pid[1],2), round(summary(model8)$varcor$cond$comID[1],2))
names(randvar.8)<-c("pid", "comID")
## ----------- ##

list8=list(est.8, or.8, sd.8, pv.8, log.8, nobs.npid.ncomID, randvar.8)
names(list8)<-c("Estimate (log odds)", "Odds ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects")
capture.output(list8, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS8.csv")
######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S9 ######### 
duration=data[!is.na(data$Pain.Days),]

model9 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location + z_sumdiags + (1|pid) + (1|comID), data=duration, family=genpois)

## Estimate (log odds) ##
est.9=round(summary(model9)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.9=round(exp(summary(model9)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.9=round(summary(model9)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.9=round(summary(model9)$coefficients$cond[,4],2)
tpvalue=pv.9
pvalue=pv.9
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.9=pvalue
## ----------- ##

## Link function ##
log.9=summary(model9)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.9=summary(model9)$nobs
nobs.npid.ncomID=c(nobs.9,summary(model9)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.9=c(round(summary(model9)$varcor$cond$pid[1],2), round(summary(model9)$varcor$cond$comID[1],2))
names(randvar.9)<-c("pid", "comID")
## ----------- ##

list9=list(est.9, or.9, sd.9, pv.9, log.9, nobs.npid.ncomID, randvar.9)
names(list9)<-c("Estimate (log odds)", "Incident rate ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects")
capture.output(list9, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS9.csv")
######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S10 ######### 
model10 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location + z_sumdiags + (1|pid) + (1|comID), data=data, family=binomial)

## Estimate (log odds) ##
est.10=round(summary(model10)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.10=round(exp(summary(model10)$coefficients$cond[,1]),3)
## ----------- ##


## SD ##
sd.10=round(summary(model10)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.10=round(summary(model10)$coefficients$cond[,4],2)
tpvalue=pv.10
pvalue=pv.10
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.10=pvalue
## ----------- ##

## Link function ##
log.10=summary(model10)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.10=summary(model10)$nobs
nobs.npid.ncomID=c(nobs.10,summary(model10)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.10=c(round(summary(model10)$varcor$cond$pid[1],2), round(summary(model10)$varcor$cond$comID[1],2))
names(randvar.10)<-c("pid", "comID")
## ----------- ##

list10=list(est.10, or.10, sd.10, pv.10, log.10, nobs.npid.ncomID, randvar.10)
names(list10)<-c("Estimate (log odds)", "Odds ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects")
capture.output(list10, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS10.csv")
######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S11 #########
ddata=data
ddata$Chronic.Pain180<-as.numeric(ddata$Chronic.Pain180)
model11 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location + z_sumdiags + (1|pid) + (1|comID), data=ddata, family=binomial)

## Estimate (log odds) ##
est.11=round(summary(model11)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.11=round(exp(summary(model11)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.11=round(summary(model11)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.11=round(summary(model11)$coefficients$cond[,4],2)
tpvalue=pv.11
pvalue=pv.11
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.11=pvalue
## ----------- ##

## Link function ##
log.11=summary(model11)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.11=summary(model11)$nobs
nobs.npid.ncomID=c(nobs.11,summary(model11)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.11=c(round(summary(model11)$varcor$cond$pid[1],2), round(summary(model11)$varcor$cond$comID[1],2))
names(randvar.11)<-c("pid", "comID")
## ----------- ##

list11=list(est.11, or.11, sd.11, pv.11, log.11, nobs.npid.ncomID, randvar.11)
names(list11)<-c("Estimate (log odds)", "Odds ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects")
capture.output(list11, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS11.csv")

# back to reference order of anatomical location
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("back","foot", "hand", "leg", "arm"))

######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE S6 #########

data$Chronic.Pain180=as.numeric(data$Chronic.Pain180)

#### 1) storing odds ratios and CIs for each pain outcome and each ICD10 diagnosis category ####
library(glmmTMB)
### a) Respiratory diagnosis ###
## Pain ##
model13 <- glmmTMB(Pain ~ z_age + male + Anatomical.location  + resp_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model13)
#compute odds ratios and cis
or=confint(model13)
anypain.resp=or[8,]
## ----------- ##

## Pain duration ##
model14 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location   + resp_diag + (1|pid) + (1|comID), data=data, family=genpois)
summary(model14)
#compute odds ratios and cis
or=confint(model14)
paindays.resp=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model15 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location   + resp_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model15)
#compute odds ratios and cis
or=confint(model15)
chropain90.resp=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model16 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location   + resp_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model16)
#compute odds ratios and cis
or=confint(model16)
chropain180.resp=or[8,]
### ------------------- ###

### b) Musculatory diagnosis ###
## Pain ##
model17 <- glmmTMB(Pain ~ z_age + male + Anatomical.location + musc_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model17)
#compute odds ratios and cis
or=confint(model17)
anypain.musc=or[8,]
## ----------- ##

## Pain duration ##
model18 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location   + musc_diag + (1|pid) + (1|comID), data=data, family=genpois)
summary(model18)
#compute odds ratios and cis
or=confint(model18)
paindays.musc=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model19 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location   + musc_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model19)
#compute odds ratios and cis
or=confint(model19)
chropain90.musc=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model20 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location   + musc_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model20)
#compute odds ratios and cis
or=confint(model20)
chropain180.musc=or[8,]
### ------------------- ###

### c) Gastrointestinal diagnosis ###
## Pain ##
model21 <- glmmTMB(Pain ~ z_age + male + Anatomical.location  + gi_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model21)
#compute odds ratios and cis
or=confint(model21)
anypain.gi=or[8,]
## ----------- ##

## Pain duration ##
model22 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location   + gi_diag + (1|pid) + (1|comID), data=data, family=genpois)
summary(model22)
#compute odds ratios and cis
or=confint(model22)
paindays.gi=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model23 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location   + gi_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model23)
#compute odds ratios and cis
or=confint(model23)
chropain90.gi=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model24 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location   + gi_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model24)
#compute odds ratios and cis
or=confint(model24)
chropain180.gi=or[8,]
### ------------------- ###

### d) Genito-urinary diagnosis ###
## Pain ##
model25 <- glmmTMB(Pain ~ z_age + male + Anatomical.location  + genito_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model25)
#compute odds ratios and cis
or=confint(model25)
anypain.genito=or[8,]
## ----------- ##

## Pain duration ##
model26 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location   + genito_diag + (1|pid) + (1|comID), data=data, family=genpois)
summary(model26)
#compute odds ratios and cis
or=confint(model26)
paindays.genito=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model27 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location   + genito_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model27)
#compute odds ratios and cis
or=confint(model27)
chropain90.genito=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model28 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location   + genito_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model28)
#compute odds ratios and cis
or=confint(model28)
chropain180.genito=or[8,]
### ------------------- ###

### e) Other infections ###
## Pain duration ##
model29 <- glmmTMB(Pain ~ z_age + male + Anatomical.location  + infection_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model29)
#compute odds ratios and cis
or=confint(model29)
anypain.infection=or[8,]
## ----------- ##

## Pain duration ##
model30 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location   + infection_diag + (1|pid) + (1|comID), data=data, family=genpois)
summary(model30)
#compute odds ratios and cis
or=confint(model30)
paindays.infection=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model31 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location   + infection_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model31)
#compute odds ratios and cis
or=confint(model31)
chropain90.infection=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model32 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location   + infection_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model32)
#compute odds ratios and cis
or=confint(model32)
chropain180.infection=or[8,]
### ------------------- ###

### f) Circulatory diagnosis ###
## Pain ##
model33 <- glmmTMB(Pain ~ z_age + male + Anatomical.location  + circulatory_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model33)
#compute odds ratios and cis
or=confint(model33)
anypain.circu=or[8,]
## ----------- ##

## Pain duration ##
model34 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location   + circulatory_diag + (1|pid) + (1|comID), data=data, family=genpois)
summary(model34)
#compute odds ratios and cis
or=confint(model34)
paindays.circu=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model35 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location   + circulatory_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model35)
#compute odds ratios and cis
or=confint(model35)
chropain90.circu=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model36 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location   + circulatory_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model36)
#compute odds ratios and cis
or=confint(model36)
chropain180.circu=or[8,]
### ------------------- ###

### g) Skin diagnosis ###
## Pain ##
model37 <- glmmTMB(Pain ~ z_age + male + Anatomical.location  + skin_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model37)
#compute odds ratios and cis
or=confint(model37)
anypain.skin=or[8,]
## ----------- ##

## Pain duration ##
model38 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location   + skin_diag + (1|pid) + (1|comID), data=data, family=genpois)
summary(model38)
#compute odds ratios and cis
or=confint(model38)
paindays.skin=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model39 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location   + skin_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model39)
#compute odds ratios and cis
or=confint(model39)
chropain90.skin=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model40 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location   + skin_diag + (1|pid) + (1|comID), data=data, family=binomial)
summary(model40)
#compute odds ratios and cis
or=confint(model40)
chropain180.skin=or[8,]
### ------------------- ###

### h) Sum of diagnoses ###
## Pain ##
model41 <- glmmTMB(Pain ~ z_age + male + Anatomical.location  + sumdiags + (1|pid) + (1|comID), data=data, family=binomial)
summary(model41)
#compute odds ratios and cis
or=confint(model41)
anypain.sumdiags=or[8,]
## ----------- ##

## Pain duration ##
model42 <- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location  + sumdiags + (1|pid) + (1|comID), data=data, family=genpois)
summary(model42)
or=confint(model42)
paindays.sumdiags=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model43 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location   + sumdiags + (1|pid) + (1|comID), data=data, family=binomial)
summary(model43)
#compute odds ratios and cis
or=confint(model43)
chropain90.sumdiags=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model44 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location   + sumdiags + (1|pid) + (1|comID), data=data, family=binomial)
summary(model44)
#compute odds ratios and cis
or=confint(model44)
chropain180.sumdiags=or[8,]
#### -------------------------- ####

#### 2) merging the stored odds ratios and CIs in a dataset ####
tor.data=rbind(anypain.resp,paindays.resp, chropain90.resp, chropain180.resp, anypain.musc, paindays.musc,chropain90.musc, chropain180.musc, anypain.gi, paindays.gi, chropain90.gi ,chropain180.gi, anypain.genito, paindays.genito, chropain90.genito, chropain180.genito, anypain.infection, paindays.infection, chropain90.infection, chropain180.infection, anypain.circu, paindays.circu, chropain90.circu, chropain180.circu, anypain.skin, paindays.skin, chropain90.skin, chropain180.skin, anypain.sumdiags, paindays.sumdiags, chropain90.sumdiags, chropain180.sumdiags)
colnames(tor.data)<-c("logit.ci_low", "logit.ci_high", "logits")
clinicaldiag=c(rep("Respiratory",4), rep("Musculoskeletal",4),rep("Gastrointestinal",4),rep("Genito-urinary",4),rep("Other infections",4),rep("Circulatory",4),rep("Skin/subcutaneous",4), rep("Summed diagnoses",4))
outcomes=rep(c("Any Pain", "Pain Duration", "Chronic Pain ≥ 3 months", "Chronic Pain ≥ 6 months"),8)
or.data=cbind(clinicaldiag, outcomes, tor.data)
#then use of or.data to produce ggplot
outcomes.levels=c("Any Pain", "Pain Duration", "Chronic Pain ≥ 3 months", "Chronic Pain ≥ 6 months")
clinical.levels=c("Respiratory" , "Musculoskeletal", "Gastrointestinal", "Genito-urinary", "Other infections", "Circulatory", "Skin/subcutaneous", "Summed diagnoses")
or.data<-as.data.frame(or.data)

or.data$logit.ci_low<-as.numeric(or.data$logit.ci_low)
or.data$logit.ci_high<-as.numeric(or.data$logit.ci_high)
or.data$logits<-as.numeric(or.data$logits)
or.data$clinicaldiag<-factor(or.data$clinicaldiag, levels=clinical.levels)
or.data$outcomes<-factor(or.data$outcomes, levels=outcomes.levels)
options(digits=1)

scaleFUN <- function(x) sprintf("%.1f", x)

fig.S6<-ggplot(or.data, aes(x=clinicaldiag)) +
  geom_point(aes(y=logits, colour = factor(outcomes, labels=outcomes.levels), shape = factor(outcomes, labels=outcomes.levels)), position = position_dodge(width= 0.6), size=8)+
  geom_errorbar(aes(ymin = logit.ci_low, ymax = logit.ci_high, colour = factor(outcomes, labels=outcomes.levels)), position = position_dodge(width= 0.6), size = 1.7)+
  xlab("ICD-10 diagnosis category") +
  ylab("Log odds or Log count") +
  theme_classic()+
  scale_shape_manual(values=c(15,16,17,18))+
  labs(shape=" ", colour=" ")+
  theme(text = element_text(size=32),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "top" )


add_y_break <- function(plot, yval) {
  
  p2 <- ggplot_build(plot)
  breaks <- p2$layout$panel_params[[1]]$y$breaks
  breaks <- breaks[!is.na(breaks)]
  plot +
    geom_hline(yintercept = yval, linetype="dotted") +
    scale_y_continuous(breaks = sort(c(yval, breaks)), labels=scaleFUN)
}

# add a dotted line at y = 0
fig.S6<-add_y_break(fig.S6, 0)

fig.S6

dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/figureS6.tiff", width=2000, height=900, units="px")
dev.off()

######### --------------------------------------------------------------------------------------------------- ######### 

######### TABLE S12 ######### 

## BASELINE ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row1=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + respiratory ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + resp_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + resp_diag + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + resp_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + resp_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row2=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + musculoskeletal ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + musc_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + musc_diag + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + musc_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + musc_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row3=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + gastrointestinal ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + gi_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + gi_diag + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + gi_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + gi_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row4=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + genitourinary ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + genito_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + genito_diag + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + genito_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + genito_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row5=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + other infection ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + infection_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + infection_diag + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + infection_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + infection_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row6=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + circulatory ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + circulatory_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + circulatory_diag + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + circulatory_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + circulatory_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row7=c(aic1, aic2, aic3, aic4)
## ----------- ##


## BASELINE + skin/subcutaneous ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + skin_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + skin_diag + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + skin_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + skin_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row8=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + summed diagnoses ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + sumdiags + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + sumdiags + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + sumdiags + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + sumdiags + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row9=c(aic1, aic2, aic3, aic4)
## ----------- ##


## BASELINE + summed diagnoses + musculoskeletal + circulatory ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + sumdiags +musc_diag +circulatory_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + sumdiags +musc_diag +circulatory_diag + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + sumdiags +musc_diag +circulatory_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + sumdiags +musc_diag +circulatory_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row10=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + + summed diagnoses + gastrointestinal + genitourinary ##
mod=glmmTMB(Pain ~ age + male + Anatomical.location + sumdiags +gi_diag + genito_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Pain.Days ~ age + male + Anatomical.location + sumdiags +gi_diag + genito_diag + (1 | pid) + (1 | comID), data=duration, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain90 ~ age + male + Anatomical.location + sumdiags +gi_diag + genito_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod=glmmTMB(Chronic.Pain180 ~ age + male + Anatomical.location + sumdiags +gi_diag + genito_diag + (1 | pid) + (1 | comID), data=data, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

row11=c(aic1, aic2, aic3, aic4)
## ----------- ##

temptable=rbind(row1, row2, row3, row4, row5, row6, row7, row8, row9, row10, row11)

list12=list(temptable)
colnames(list12[[1]])<-c("1—Current pain", "2--Pain duration", "3a) Chronic Pain ≥3 months", "3b) Chronic Pain ≥6 months")
rownames(list12[[1]])<-c("Baseline: Pain outcome ~ age + male + anatomical location + (1 | pid) + (1 | comID)", "Baseline + respiratory", "Baseline + musculoskeletal", "Baseline + gastrointestinal
", "Baseline + genitourinary", "Baseline + other infection", "Baseline + circulatory", "Baseline + skin/subcutaneous", "Baseline + summed diagnoses", "Baseline + summed diagnoses + musculoskeletal + circulatory", "Baseline + summed diagnoses + gastrointestinal + genitourinary")

capture.output(list12, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS12.csv")

# back to reference order of anatomical location
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("back","foot", "hand", "leg", "arm"))

######### --------------------------------------------------------------------------------------------------- ######### 

######### FIGURE S7 ######### 
#selecting females
dataf=data[data$male=="0",]

#### 1) storing odds ratios and CIs for each pain outcome and each indicator of female reproductive effort ####
library(glmmTMB)
### a) z_afb : Age at first birth ###
## Pain ##
model13 <- glmmTMB(Pain ~ z_age    + Anatomical.location +z_sumdiags  + z_afb + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model13)
#compute odds ratios and cis
or=confint(model13)

anypain.afb=or[8,]
## ----------- ##

## Pain duration ##
model14 <- glmmTMB(Pain.Days ~ z_age    + Anatomical.location +z_sumdiags   + z_afb + (1|pid) + (1|comID), data = dataf, family=genpois)
summary(model14)
#compute odds ratios and cis
or=confint(model14)

paindays.afb=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model15 <- glmmTMB(Chronic.Pain90 ~ z_age    + Anatomical.location +z_sumdiags   + z_afb + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model15)
#compute odds ratios and cis
or=confint(model15)

chropain90.afb=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model16 <- glmmTMB(Chronic.Pain180 ~ z_age    + Anatomical.location +z_sumdiags   + z_afb + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model16)
#compute odds ratios and cis
or=confint(model16)

chropain180.afb=or[8,]
### ------------------- ###

### b) z_meanibi : Mean Interbirth Interval ###
## Pain ##
model17 <- glmmTMB(Pain ~ z_age    + Anatomical.location +z_sumdiags + z_meanibi + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model17)
#compute odds ratios and cis
or=confint(model17)

anypain.meanibi=or[8,]
## ----------- ##

## Pain duration ##
model18 <- glmmTMB(Pain.Days ~ z_age    + Anatomical.location +z_sumdiags   + z_meanibi + (1|pid) + (1|comID), data = dataf, family=genpois)
summary(model18)
#compute odds ratios and cis
or=confint(model18)

paindays.meanibi=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model19 <- glmmTMB(Chronic.Pain90 ~ z_age    + Anatomical.location +z_sumdiags   + z_meanibi + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model19)
#compute odds ratios and cis
or=confint(model19)

chropain90.meanibi=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model20 <- glmmTMB(Chronic.Pain180 ~ z_age    + Anatomical.location +z_sumdiags   + z_meanibi + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model20)
#compute odds ratios and cis
or=confint(model20)

chropain180.meanibi=or[8,]
### ------------------- ###


### c) z_totalkids : Number of live births ###
## Pain ##
model21 <- glmmTMB(Pain ~ z_age    + Anatomical.location +z_sumdiags  + z_totalkids + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model21)
#compute odds ratios and cis
or=confint(model21)

anypain.totalkid=or[8,]
## ----------- ##

## Pain duration ##
model22 <- glmmTMB(Pain.Days ~ z_age    + Anatomical.location +z_sumdiags   + z_totalkids + (1|pid) + (1|comID), data = dataf, family=genpois)
summary(model22)
#compute odds ratios and cis
or=confint(model22)

paindays.totalkid=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model23 <- glmmTMB(Chronic.Pain90 ~ z_age    + Anatomical.location +z_sumdiags   + z_totalkids + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model23)
#compute odds ratios and cis
or=confint(model23)

chropain90.totalkid=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model24 <- glmmTMB(Chronic.Pain180 ~ z_age    + Anatomical.location +z_sumdiags   + z_totalkids + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model24)
#compute odds ratios and cis
or=confint(model24)

chropain180.totalkid=or[8,]

### d) kidunder1 : Gave birth in past year (ref: no) ###
## Pain ##
model25 <- glmmTMB(Pain ~ z_age    + Anatomical.location +z_sumdiags  + kidunder1 + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model25)
#compute odds ratios and cis
or=confint(model25)

anypain.kidunder1=or[8,]
## ----------- ##

## Pain duration ##
model26 <- glmmTMB(Pain.Days ~ z_age    + Anatomical.location +z_sumdiags   + kidunder1 + (1|pid) + (1|comID), data = dataf, family=genpois)
summary(model26)
#compute odds ratios and cis
or=confint(model26)

paindays.kidunder1=or[8,]
## ----------- ##

## Chronic Pain >= 90 days ##
model27 <- glmmTMB(Chronic.Pain90 ~ z_age    + Anatomical.location +z_sumdiags   + kidunder1 + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model27)
#compute odds ratios and cis
or=confint(model27)

chropain90.kidunder1=or[8,]
## ----------- ##

## Chronic Pain >= 180 days ##
model28 <- glmmTMB(Chronic.Pain180 ~ z_age    + Anatomical.location +z_sumdiags   + kidunder1 + (1|pid) + (1|comID), data = dataf, family=binomial)
summary(model28)
#compute odds ratios and cis
or=confint(model28)

chropain180.kidunder1=or[8,]
#### ------------------------------------------------------------- ####

#### 2) merging the stored odds ratios and CIs in a dataset ####
tor.dataf=rbind(anypain.afb,paindays.afb, chropain90.afb, chropain180.afb, anypain.meanibi, paindays.meanibi,chropain90.meanibi, chropain180.meanibi, anypain.totalkid, paindays.totalkid, chropain90.totalkid ,chropain180.totalkid, anypain.kidunder1, paindays.kidunder1, chropain90.kidunder1, chropain180.kidunder1)
colnames(tor.dataf)<-c("logit.ci_low", "logit.ci_high", "logits")
clinicaldiag=c(rep("Age at first birth",4), rep("Mean interbirth interval",4),rep("Number of live births",4),rep("Gave birth in past year (ref: no)",4))
outcomes=rep(c("Any Pain", "Pain Duration", "Chronic Pain ≥ 3 months", "Chronic Pain ≥ 6 months"),4)
or.dataf=cbind(clinicaldiag, outcomes, tor.dataf)
#then use of or.dataf to produce ggplot
outcomes.levels=c("Any Pain", "Pain Duration", "Chronic Pain ≥ 3 months", "Chronic Pain ≥ 6 months")
clinical.levels=c("Age at first birth" , "Mean interbirth interval", "Number of live births", "Gave birth in past year (ref: no)")
or.dataf<-as.data.frame(or.dataf)

or.dataf$logit.ci_low<-as.numeric(or.dataf$logit.ci_low)
or.dataf$logit.ci_high<-as.numeric(or.dataf$logit.ci_high)
or.dataf$logits<-as.numeric(or.dataf$logits)
or.dataf$clinicaldiag<-factor(or.dataf$clinicaldiag, levels=clinical.levels)
or.dataf$outcomes<-factor(or.dataf$outcomes, levels=outcomes.levels)

library(ggplot2)
fig.S7<-ggplot(or.dataf, aes(x=clinicaldiag)) +
  geom_point(aes(y=logits, colour = factor(outcomes, labels=outcomes.levels), shape = factor(outcomes, labels=outcomes.levels)), position = position_dodge(width= 0.6), size=8)+
  geom_errorbar(aes(ymin = logit.ci_low, ymax = logit.ci_high, colour = factor(outcomes, labels=outcomes.levels)), position = position_dodge(width= 0.6), size = 1.7)+
  xlab(" Indicator of female reproductive effort") +
  ylab("Log odds or Log count") +
  theme_classic()+
  scale_shape_manual(values=c(15,16,17,18))+
  labs(shape=" ", colour=" ")+
  theme(text = element_text(size=46),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),legend.position = "top" )

# add a dotted line at y = 0
fig.S7<-add_y_break(fig.S7, 0)

fig.S7

dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/figureS7.tiff", width=2000, height=900, units="px")
dev.off()

######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S13 ######### 
# coding arm as the reference 
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("arm", "back","foot", "hand", "leg"))
duration=data[!is.na(data$Pain.Days),]

## BASELINE: Pain outcome ~ age + anatomical location + sum of illness categories + (1 | pid) + (1 | comID) ##
mod <- glmmTMB(Pain ~ z_age    + Anatomical.location +z_sumdiags + (1|pid) + (1|comID), data = dataf, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod <- glmmTMB(Pain.Days ~ z_age    + Anatomical.location +z_sumdiags + (1|pid) + (1|comID), data = dataf, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod<- glmmTMB(Chronic.Pain90 ~ z_age    + Anatomical.location +z_sumdiags  + (1|pid) + (1|comID), data = dataf, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod<- glmmTMB(Chronic.Pain180 ~ z_age    + Anatomical.location +z_sumdiags  + (1|pid) + (1|comID), data = dataf, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")
row1=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + age at first birth ##
mod <- glmmTMB(Pain ~ z_age    + Anatomical.location +z_sumdiags + afb+ (1|pid) + (1|comID), data = dataf, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod <- glmmTMB(Pain.Days ~ z_age    + Anatomical.location +z_sumdiags + afb+ (1|pid) + (1|comID), data = dataf, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod<- glmmTMB(Chronic.Pain90 ~ z_age    + Anatomical.location +z_sumdiags + afb + (1|pid) + (1|comID), data = dataf, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod<- glmmTMB(Chronic.Pain180 ~ z_age    + Anatomical.location +z_sumdiags + afb + (1|pid) + (1|comID), data = dataf, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")
row2=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + mean interbirth interval ##
mod <- glmmTMB(Pain ~ z_age    + Anatomical.location +z_sumdiags + mean_ibi_mos+ (1|pid) + (1|comID), data = dataf, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod <- glmmTMB(Pain.Days ~ z_age    + Anatomical.location +z_sumdiags + mean_ibi_mos+ (1|pid) + (1|comID), data = dataf, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod<- glmmTMB(Chronic.Pain90 ~ z_age    + Anatomical.location +z_sumdiags + mean_ibi_mos + (1|pid) + (1|comID), data = dataf, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod<- glmmTMB(Chronic.Pain180 ~ z_age    + Anatomical.location +z_sumdiags + mean_ibi_mos + (1|pid) + (1|comID), data = dataf, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")
row3=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE +  number of live births ##
mod <- glmmTMB(Pain ~ z_age    + Anatomical.location +z_sumdiags + totalkids+ (1|pid) + (1|comID), data = dataf, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod <- glmmTMB(Pain.Days ~ z_age    + Anatomical.location +z_sumdiags + totalkids+ (1|pid) + (1|comID), data = dataf, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod<- glmmTMB(Chronic.Pain90 ~ z_age    + Anatomical.location +z_sumdiags + totalkids + (1|pid) + (1|comID), data = dataf, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod<- glmmTMB(Chronic.Pain180 ~ z_age    + Anatomical.location +z_sumdiags + totalkids + (1|pid) + (1|comID), data = dataf, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")
row4=c(aic1, aic2, aic3, aic4)
## ----------- ##

## BASELINE + gave birth in past year (reference: no) ##
mod <- glmmTMB(Pain ~ z_age    + Anatomical.location +z_sumdiags + kidunder1+ (1|pid) + (1|comID), data = dataf, family=binomial)
aic1=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod <- glmmTMB(Pain.Days ~ z_age    + Anatomical.location +z_sumdiags + kidunder1+ (1|pid) + (1|comID), data = dataf, family=genpois)
aic2=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod<- glmmTMB(Chronic.Pain90 ~ z_age    + Anatomical.location +z_sumdiags + kidunder1 + (1|pid) + (1|comID), data = dataf, family=binomial)
aic3=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")

mod<- glmmTMB(Chronic.Pain180 ~ z_age    + Anatomical.location +z_sumdiags + kidunder1 + (1|pid) + (1|comID), data = dataf, family=binomial)
aic4=paste0(round(as.numeric(summary(mod)$AICtab[1]),1), " (", round(as.numeric(summary(mod)$AICtab[3]),1),")")
row5=c(aic1, aic2, aic3, aic4)
## ----------- ##

temptable2=rbind(row1, row2, row3, row4, row5)

list13=list(temptable2)
colnames(list13[[1]])<-c("1—Current pain", "2--Pain duration", "3a) Chronic Pain ≥3 months", "3b) Chronic Pain ≥6 months")
rownames(list13[[1]])<-c("Baseline: Pain outcome ~ age + anatomical location + sum of illness categories + (1 | pid) + (1 | comID)", "Baseline + age at first birth", "Baseline + mean interbirth interval", "Baseline + number of live births", "Baseline + gave birth in past year (reference: no)")

capture.output(list13, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS13.csv")

######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S14 ######### 
model14 <- glmmTMB(Pain ~ z_age + male + Anatomical.location + z_sumdiags + z_YearsSchool + (1|pid) + (1|comID), data=data, family=binomial)


## Estimate (log odds) ##
est.14=round(summary(model14)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.14=round(exp(summary(model14)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.14=round(summary(model14)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.14=round(summary(model14)$coefficients$cond[,4],2)
tpvalue=pv.14
pvalue=pv.14
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.14=pvalue
## ----------- ##

## Link function ##
log.14=summary(model14)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.14=summary(model14)$nobs
nobs.npid.ncomID=c(nobs.14,summary(model14)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.14=c(round(summary(model14)$varcor$cond$pid[1],2), round(summary(model14)$varcor$cond$comID[1],2))
names(randvar.14)<-c("pid", "comID")
## ----------- ##

list14=list(est.14, or.14, sd.14, pv.14, log.14, nobs.npid.ncomID, randvar.14)
names(list14)<-c("Estimate (log odds)", "Odds ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects")
capture.output(list14, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS14.csv")
######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S15 #########
model15<- glmmTMB(Pain.Days ~ z_age + male + Anatomical.location + z_sumdiags + z_YearsSchool + (1|pid) + (1|comID), data=duration, family=genpois, ziformula= ~ 1)
summary(model15)

## Estimate (log odds) ##
est.15=round(summary(model15)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.15=round(exp(summary(model15)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.15=round(summary(model15)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.15=round(summary(model15)$coefficients$cond[,4],2)
tpvalue=pv.15
pvalue=pv.15
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.15=pvalue
## ----------- ##

## Link function ##
log.15=summary(model15)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.15=summary(model15)$nobs
nobs.npid.ncomID=c(nobs.15,summary(model15)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variables ##
randvar.15=c(round(summary(model15)$varcor$cond$pid[1],2), round(summary(model15)$varcor$cond$comID[1],2))
names(randvar.15)<-c("pid", "comID")
## ----------- ##

## Zero inflation model ##
## Estimate (log odds) ##
est.zim.15=round(summary(model15)$coefficients$zi[,1],3)
## ----------- ##

## Odds ratio ##
or.zim.15=round(exp(summary(model15)$coefficients$zi[,1]),3)          
## ----------- ##

## SD ##
sd.zim.15=round(summary(model15)$coefficients$zi[,2],2)
## ----------- ##

## P-values ##
pv.zim.15=round(summary(model15)$coefficients$zi[,4],2)
tpvalue=pv.zim.15
pvalue=pv.zim.15
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.zim.15=pvalue
## ----------- ##

list15=list(est.15, or.15, sd.15, pv.15, log.15, nobs.npid.ncomID, randvar.15, est.zim.15, or.zim.15, sd.zim.15, pv.zim.15)
names(list15)<-c("Estimate (log odds)", "Incident rate ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects", "Estimate (log odds)", "Incident rate ratio", "Standard deviations", "P-values")
capture.output(list15, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS15.csv")
######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S16 ######### 
model16 <- glmmTMB(Chronic.Pain90 ~ z_age + male + Anatomical.location + z_sumdiags + z_YearsSchool + (1|pid) + (1|comID), data=data, family=binomial)

## Estimate (log odds) ##
est.16=round(summary(model16)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.16=round(exp(summary(model16)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.16=round(summary(model16)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.16=round(summary(model16)$coefficients$cond[,4],2)
tpvalue=pv.16
pvalue=pv.16
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.16=pvalue
## ----------- ##

## Link function ##
log.16=summary(model16)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.16=summary(model16)$nobs
nobs.npid.ncomID=c(nobs.16,summary(model16)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.16=c(round(summary(model16)$varcor$cond$pid[1],2), round(summary(model16)$varcor$cond$comID[1],2))
names(randvar.16)<-c("pid", "comID")
## ----------- ##


list16=list(est.16, or.16, sd.16, pv.16, log.16, nobs.npid.ncomID, randvar.16)
names(list16)<-c("Estimate (log odds)", "Odds ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects")
capture.output(list16, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS16.csv")

######### --------------------------------------------------------------------------------------------------- ######### 


######### TABLE S17 ######### 
ddata=data
ddata$Chronic.Pain180<-as.numeric(ddata$Chronic.Pain180)

model17 <- glmmTMB(Chronic.Pain180 ~ z_age + male + Anatomical.location + z_sumdiags + z_YearsSchool + (1|pid) + (1|comID), data=ddata, family=binomial)
summary(model17)

## Estimate (log odds) ##
est.17=round(summary(model17)$coefficients$cond[,1],3)
## ----------- ## 

## Odds ratio ##
or.17=round(exp(summary(model17)$coefficients$cond[,1]),3)
## ----------- ##

## SD ##
sd.17=round(summary(model17)$coefficients$cond[,2],2)
## ----------- ##

## P-values ##
pv.17=round(summary(model17)$coefficients$cond[,4],2)
tpvalue=pv.17
pvalue=pv.17
for (i in 1:length(tpvalue))
{
  if (tpvalue[i]<=0.001)
  {
    pvalue[i]<-"p≤0.001"
  }
  if (tpvalue[i]<=0.01 & tpvalue[i]>0.001)
  {
    pvalue[i]<-"p≤0.01"
  }
  if (tpvalue[i]<=0.05 & tpvalue[i]>0.01)
  {
    pvalue[i]<-"p≤0.05"
  }
  if (tpvalue[i]<=0.1 & tpvalue[i]>0.05)
  {
    pvalue[i]<-"p≤0.1"
  }
  if (tpvalue[i]>0.1)
  {
    pvalue[i]<-round(tpvalue[i], 3)
  }
}
pvalue
pv.17=pvalue
## ----------- ##

## Link function ##
log.17=summary(model17)$family
## ----------- ##

## Number of person-observations and number of individuals (pid) ##
nobs.17=summary(model17)$nobs
nobs.npid.ncomID=c(nobs.17,summary(model17)$ngrps$cond)
names(nobs.npid.ncomID)<-c("Number of person-observations", "pid", "comID")
## ----------- ##

## Random effects variances ##
randvar.17=c(round(summary(model17)$varcor$cond$pid[1],2), round(summary(model17)$varcor$cond$comID[1],2))
names(randvar.17)<-c("pid", "comID")
## ----------- ##

list17=list(est.17,or.17, sd.17, pv.17, log.17, nobs.npid.ncomID, randvar.17)
names(list17)<-c("Estimate (log odds)", "Odds ratio", "Standard deviations", "P-values", "Logit", " ", "Variance explained by random effects")
capture.output(list17, file="/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/TABLES/tableS17.csv")

# back to reference order of anatomical location
data$Anatomical.location<-factor(data$Anatomical.location, levels=c("back","foot", "hand", "leg", "arm"))

######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE S8 #########

dddata=data[!data$cause.work=="NA",]
dddata=dddata[!dddata$cause.work=="Nonspecific",] # excluding rows that have "Nonspecific" as a work-related cause of pain
dddata$cause.work<-factor(dddata$cause.work, levels = c("Horticulture", "Overload", "Hunt or forage", "Transport", "Wood", "Fish", "Domestic")) # specifying the order of priority for the categories of cause.work for later plotting

tdlab=dddata[!is.na(dddata$YearsSchool),]

## 1) prepare dataset ##
tdlab$YearsSchool<-as.character(tdlab$YearsSchool)

tdlab[tdlab$YearsSchool=="0.083333333",72]<-"0"
tdlab[tdlab$YearsSchool=="0.666666667",72]<-"1"
tdlab[tdlab$YearsSchool=="0.333333333",72]<-"0"
tdlab[tdlab$YearsSchool=="7",72]<-"7+"
tdlab[tdlab$YearsSchool=="9",72]<-"7+"
tdlab[tdlab$YearsSchool=="13",72]<-"7+"

# storing the correct order of each age category text in order to display it on the figure 
vec=c("0", "1", "2", "3", "4", "5", "6", "7+")
dlabdata=tdlab
dlabdata$YearsSchool<-factor(dlabdata$YearsSchool, levels=vec)
## ----------- ##

## 2) coding of the plot ##
#find colorblind friendly palette
palette.colors(palette = "Alphabet")
# specifying colours for each category of work
colfill2 <- c("Overload" = "#325A9B",  "Hunt or forage" = "#F6222E", "Horticulture" = "#1C8356", "Transport" = "#B00068", "Fish" = "#F7E1A0", "Domestic" = "#FEAF16", "Wood"="#1CBE4F")

fig.S8 <- ggplot(dlabdata, aes(x = YearsSchool)) +
  theme_classic()+
  geom_bar(aes(fill = cause.work), position = "fill",  width=0.8) +
  theme(legend.position = "right")+
  labs(fill="Attributions")+
  ylab("Percent of work-related pain attributions") +
  xlab("Years of schooling") +
  theme(text = element_text(size=55),plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values = colfill2)+
  scale_y_continuous(labels = scales::percent)


fig.S8
dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/figureS8.tiff", width=1000, height=1150, units="px")
dev.off()

######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE S9 ######### 

dddata=data[!data$cause.fall.omit.nonspecific=="NA",]
unique(dddata$cause.fall.omit.nonspecific)
tdlab=dddata[!is.na(dddata$cause.fall.omit.nonspecific),]

## 1) prepare dataset ##
# Pain attribution relabelling
tdlab$cause.fall.omit.nonspecific<-as.character(tdlab$cause.fall.omit.nonspecific)

tdlab[tdlab$cause.fall.omit.nonspecific=="bathe",67]<-"Bathe"
tdlab[tdlab$cause.fall.omit.nonspecific=="carry roofing leaves",67]<-"Carry roofing leaves"
tdlab[tdlab$cause.fall.omit.nonspecific=="carry water",67]<-"Carry water"
tdlab[tdlab$cause.fall.omit.nonspecific=="chop wood",67]<-"Chop wood"
tdlab[tdlab$cause.fall.omit.nonspecific=="climb tree",67]<-"Climb tree"
tdlab[tdlab$cause.fall.omit.nonspecific=="drunk",67]<-"Drunk"
tdlab[tdlab$cause.fall.omit.nonspecific=="harvest/carry cultigen",67]<-"Harvest or carry cultigen"
tdlab[tdlab$cause.fall.omit.nonspecific=="overload",67]<-"Overload"
tdlab[tdlab$cause.fall.omit.nonspecific=="walk",67]<-"Walk"

# storing the correct order of each age category text in order to display it on the figure 
vec=c("Bathe", "Carry roofing leaves", "Carry water", "Chop wood", "Climb tree", "Drunk", "Harvest or carry cultigen", "Overload", "Walk")

# Years of schooling - relabelling
tdlab$YearsSchool<-as.character(tdlab$YearsSchool)

tdlab[tdlab$YearsSchool=="0",72]<-"0-2"
tdlab[tdlab$YearsSchool=="0.083333333",72]<-"0-2"
tdlab[tdlab$YearsSchool=="0.333333333",72]<-"0-2"
tdlab[tdlab$YearsSchool=="0.666666667",72]<-"0-2"
tdlab[tdlab$YearsSchool=="1",72]<-"0-2"
tdlab[tdlab$YearsSchool=="2",72]<-"0-2"

tdlab[tdlab$YearsSchool=="3",72]<-"3+"
tdlab[tdlab$YearsSchool=="4",72]<-"3+"
tdlab[tdlab$YearsSchool=="5",72]<-"3+"
tdlab[tdlab$YearsSchool=="6",72]<-"3+"
tdlab[tdlab$YearsSchool=="7",72]<-"3+"
tdlab[tdlab$YearsSchool=="9",72]<-"3+"
tdlab[tdlab$YearsSchool=="12",72]<-"3+"
tdlab[tdlab$YearsSchool=="13",72]<-"3+"

# storing the correct order of each age category text in order to display it on the figure 
vec2=c("0-2", "3+")

dlabdata=tdlab
dlabdata$cause.fall.omit.nonspecific<-factor(dlabdata$cause.fall.omit.nonspecific, levels=vec)
dlabdata$YearsSchool<-factor(dlabdata$YearsSchool, levels=vec2)
## ----------- ##

## 2) coding of the plot ##
# specifying colours for each category of work
colfill2 <- c("Bathe" = "cadetblue2",  "Carry roofing leaves" = "dodgerblue3", "Carry water" = "darkolivegreen1", "Chop wood" = "darkolivegreen4", "Climb tree" = "coral", "Drunk" = "red3", "Harvest or carry cultigen"="gold", "Overload"="darkorange2", "Walk"="plum")

fig.S9 <- ggplot(dlabdata, aes(x = YearsSchool)) +
  theme_classic()+
  geom_bar(aes(fill = cause.fall.omit.nonspecific), position = "fill",  width=0.8) +
  theme(legend.position = "right")+
  labs(fill="Attributions")+
  ylab("Percent of work-related pain attributions") +
  xlab("Years of schooling") +
  theme(text = element_text(size=55),plot.title = element_text(hjust = 0.5), panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_fill_manual(values = colfill2)+
  scale_y_continuous(labels = scales::percent)


fig.S9
dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/figureS9.tiff", width=1000, height=1150, units="px")
dev.off()

######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE 3 ######### 
# import new dataset
tdata <- read.csv("~/Bureau/PAIN TSIMANE REVIEWER VERSION/FILES/figure3_data.csv")

## 1) divide the dataset in male and female subsets ##
# male  
male.data<-tdata[,1:6]
male.URcountry=paste0(male.data$Setting, " \n ", male.data$Country) # merge the name of setting (rural,urban, or urban/rural), and the name of the country
male.data<-cbind(male.data,male.URcountry)
colnames(male.data)<-c("Country","Setting","Pain.cases.per.1.000.pop","lower","upper","sex","URcountry")
male.data$sex<-"Male"

# female
fem.data<- tdata[,10:15]
colnames(fem.data)<-colnames(male.data)[1:6] # merge the name of setting (rural,urban, or urban/rural), and the name of the country
fem.URcountry=paste0(fem.data$Setting, " \n ", fem.data$Country)
fem.data<-cbind(fem.data, fem.URcountry)
colnames(fem.data)<-c("Country","Setting","Pain.cases.per.1.000.pop","lower","upper","sex","URcountry")
fem.data$sex<-"Female"
## ---------------##

## 2) merging male and female subsets to form "data", the dataset used in the plot
library(tibble)
data<-tibble(rbind(male.data, fem.data))
data$Pain.cases.per.1.000.pop<-as.numeric(data$Pain.cases.per.1.000.pop)
data[data$URcountry=="U \n Bangladesh-affluent",7]<-"U \n Bangladesh\n-affluent"
male.data[male.data$URcountry=="U \n Bangladesh-affluent",7]<-"U \n Bangladesh\n-affluent"
fem.data[fem.data$URcountry=="U \n Bangladesh-affluent",7]<-"U \n Bangladesh\n-affluent"

# sorting variable by decreasing mean values of male prevalence of pain by setting/country
test<-male.data[order(male.data$Pain.cases.per.1.000.pop, decreasing=TRUE),]

levels.country<- test$URcountry # storing the decreasing order of mean values of male prevalence of pain by setting/country
data$lower<-data$Pain.cases.per.1.000.pop-data$lower # adding lower limit of CI
data$upper<-data$Pain.cases.per.1.000.pop+data$upper # adding upper limit of CI
## ---------------##

## 3) coding of the plot ##
library(ggplot2)

ggplot(data,aes(x=factor(URcountry, level=levels.country),y=Pain.cases.per.1.000.pop, fill=factor(sex)) )+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar( aes(ymin=lower, ymax=upper),stat="identity", position = position_dodge(width = 0.9),width=0.5, alpha = 0.8) +
  scale_fill_discrete(name="")+
  scale_color_discrete(name="",
                       breaks=c(1, 2),
                       labels=c("Male", "Female"))+
  xlab("Population")+ylab("Cases per 1,000 population")+
  theme_classic()+
  scale_y_continuous(limits = c(0, 1000), breaks = seq(0, 1000, by = 100))+
  theme(text = element_text(size=18), legend.position="top" )


dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/Figure3.tiff", width=1000, height=559, units="px")
dev.off()
######### --------------------------------------------------------------------------------------------------- ######### 


######### FIGURE 4 ######### 
rm(list=ls())
library(readxl)
# import new dataset
tttdata <- read.csv("~/Bureau/PAIN TSIMANE REVIEWER VERSION/FILES/figure4_data.csv")
ttdata=tttdata[c(1:5,8:11),] # remove lines 6 and 7 full of NAs

## 1) divide ttdata into male and female subsets ##
# male
tmale.ttdata=ttdata[,1:7]
male.data=tmale.ttdata[2:9,]# remove the first line 
colnames(male.data)<-tmale.ttdata[1,] # the removed first line is used as names for the columns of the dataset
colnames(male.data)[1]<-"PainMonths" # modifying the first column that specify the duration of pain
male.data[1:4,1]<- "≥ 3 months"
male.data[5:8,1]<- "≥ 6 months"
male.data$sex<-"Male"

# female
tfemale.ttdata=ttdata[,10:16]
female.data=tfemale.ttdata[2:9,]
colnames(female.data)<-tfemale.ttdata[1,]
colnames(female.data)[1]<-"PainMonths"
female.data[1:4,1]<- "≥ 3 months"
female.data[5:8,1]<- "≥ 6 months"
female.data$sex<-"Female"
## ---------------##

## 2) merging the two datasets in "data", that will be used to produce the plot ##
tdata=rbind(female.data, male.data)
URcountry=paste0(tdata$Setting, " \n ", tdata$Country) # merge the name of setting (rural,urban, or urban/rural), "/n" that insert a line break on the plot, and the name of the country
data=tibble(cbind(tdata,URcountry))
data$`Pain cases per 1,000 pop`<-as.numeric(data$`Pain cases per 1,000 pop`)
# computing lower and upper limits of the CIs
data$lower<-as.numeric(data$`Pain cases per 1,000 pop`)-as.numeric(data$lower)
data$upper<-as.numeric(data$`Pain cases per 1,000 pop`)+as.numeric(data$upper)
## ---------------##

## 3) in order to have two panels in the figure with the correct order of countries, creation of the column PainMonth.URcountry that will be used to specify the order of the countries in the plot
PainMonth.URcountry=paste0(data$PainMonths, " / ", data$URcountry) # merge the duration of pain with the previously merged character string composed of the name of setting (rural,urban, or urban/rural), "/n" that insert a line break on the plot, and the name of the country.
data=cbind(data, PainMonth.URcountry)
colnames(data)[9]<-"PainMonthURcountry"
## ---------------##

## 4) data$PainMonth.URcountry needs to have a specified order on the figure ##
#using male.data to produce levels.country, the setting/country decreasing order based on the mean values of male prevalence of pain that will be the reference ##
male.URcountry=paste0(male.data$Setting, " \n ", male.data$Country) # merge the name of setting (rural,urban, or urban/rural), "/n" that insert a line break on the plot, and the name of the country
male.data=cbind(male.data, male.URcountry)
colnames(male.data)[8]<-"URcountry"
male.URcountry=paste0(male.data$PainMonths, " / ", male.data$URcountry) # merge the duration of pain with the previously merged character string composed of the name of setting (rural,urban, or urban/rural), "/n" that insert a line break on the plot, and the name of the country.
male.data=cbind(male.data, male.URcountry)
colnames(male.data)[9]<-"PainMonthURcountry"
test<-male.data[order(male.data$`Pain cases per 1,000 pop`, decreasing=TRUE),] # order the dataset by mean decreasing order of male prevalence of pain.
levels.country<- test$PainMonthURcountry # store the new order of PainMonthURcountry.
## ---------------##

## 5) coding of the plot ##
library(ggplot2)
titlesxaxis=paste0("'",PainMonth.URcountry, "' = '", URcountry, "'") # creating the equivalence between the coordinates of each bar on the x axis "PainMonth.URcountry", and the corresponding caption on the axis "URcountry" (values of the vector copy pasted and used in scale_x_discrete() below)

ggplot(data, aes(x=factor(PainMonth.URcountry, level=levels.country),y=`Pain cases per 1,000 pop`, fill=factor(sex)) )+ # plot showing values of pain cases (y axis) organized by PainMonth.URcountry (x axis) in the order stored in levels.country
  facet_wrap(vars(PainMonths), strip.position="bottom", scales = "free_x")+ # plot divided into two panels, ≥ 3 months and ≥ 6 months
  geom_bar(stat="identity", position="dodge")+
  geom_errorbar( aes(ymin=lower, ymax=upper),stat="identity", position = position_dodge(width = 0.9),width=0.5, alpha = 0.8) + # adding CIs
  # scale_x_discrete() allows to specify the equivalence between the coordinates of each bar on the x axis "PainMonth.URcountry", and the corresponding caption on the axis "URcountry"
  scale_x_discrete(labels = c('≥ 3 months / UR \n Sweden' = 'UR \n Sweden', '≥ 3 months / UR \n Scotland' = 'UR \n Scotland', '≥ 3 months / R \n Tsimane' = 'R \n Tsimane', '≥ 3 months / UR \n Ireland' = 'UR \n Ireland', '≥ 6 months / U \n Canada' = 'U \n Canada', '≥ 6 months / R \n Tsimane' = 'R \n Tsimane', '≥ 6 months / UR \n USA' = 'UR \n USA', '≥ 6 months / UR \n Denmark' = 'UR \n Denmark'  ) )+
  scale_fill_discrete(name="")+
  scale_color_discrete(name="",
                       breaks=c(1, 2),
                       labels=c("Male", "Female"))+
  xlab("Population and chronic pain definition")+ylab("Cases per 1,000 population")+
  scale_y_continuous(limits = c(0, 600), breaks = seq(0, 600, by = 100))+
  theme_classic()+
  theme(text = element_text(size=25), legend.position="top" )


dev.print(tiff, "/home/yoann/Bureau/PAIN TSIMANE REVIEWER VERSION/FIGURES/Figure4.tiff", width=1000, height=559, units="px")
dev.off()
######### --------------------------------------------------------------------------------------------------- ######### 