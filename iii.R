setwd("D:/University/Video semester2 year1/ctrr/btl/CTRR")
covid.data <- read.csv("R/owid-covid-data.csv")
name.country<-c("Brazil", "Chile", "Venezuela")
#############################################################
#1)number of days without report

#new cases#
ndwithoutrpcases <-function(name){
  select <- covid.data$location == name
  covid.data.location <-subset(covid.data, subset = select)
  ndays <-0
  for(i in 1:nrow(covid.data.location)){
    if(is.na(covid.data.location$new_cases[i]))
      ndays <- ndays +1
  }
  print(ndays)
}
#Brazil
ndwithoutrpcases(name.country[1])

#Chile
ndwithoutrpcases(name.country[2])

#Venezuela
ndwithoutrpcases(name.country[3])

#new deaths#
ndwithoutrpdeaths <-function(name){
  select <- covid.data$location == name
  ndays <-0
  covid.data.location <-subset(covid.data, subset = select)
  for(i in 1:nrow(covid.data.location)){
    if(is.na(covid.data.location$new_deaths[i]))
      ndays <- ndays +1
  }
  print(ndays)
}
#Brazil
ndwithoutrpdeaths(name.country[1])

#Chile
ndwithoutrpdeaths(name.country[2])

#Venezuela
ndwithoutrpdeaths(name.country[3])

#############################################################
#2)number of lowest days at that time

#new cases#
nlowestdayscases <-function(name){
  select <- covid.data$location == name
  covid.data.location <-subset(covid.data, subset = select)
  ndays <-0
  min<-0
  for(i in 1:nrow(covid.data.location))
  {
    if(!is.na(covid.data.location$new_cases[i])){
      min<-covid.data.location$new_cases[i]
      break
    }
  }
  for(i in 1:nrow(covid.data.location))
  {
    if(is.na(covid.data.location$new_cases[i])){
      next
    }
    if(min>covid.data.location$new_cases[i]){
      min <- covid.data.location$new_cases[i]
      ndays <- ndays +1
    }
  }
  print(ndays)
}
#Brazil
nlowestdayscases(name.country[1])
#Chile
nlowestdayscases(name.country[2])
#Venezuela
nlowestdayscases(name.country[3])

#new deaths#
nlowestdaysdeaths <-function(name){
  select <- covid.data$location == name
  covid.data.location <-subset(covid.data, subset = select)
  ndays <-0
  min<-0
  for(i in 1:nrow(covid.data.location))
  {
    if(!is.na(covid.data.location$new_deaths[i])){
      min<-covid.data.location$new_deaths[i]
      break
    }
  }
  for(i in 1:nrow(covid.data.location))
  {
    if(is.na(covid.data.location$new_deaths[i])){
      next
    }
    if(min>covid.data.location$new_deaths[i]){
      min <- covid.data.location$new_deaths[i]
      ndays <- ndays +1
    }
  }
  print(ndays)
}
#Brazil
nlowestdaysdeaths(name.country[1])
#Chile
nlowestdaysdeaths(name.country[2])
#Venezuela
nlowestdaysdeaths(name.country[3])

#############################################################
#3)number of highest days at that time

#new cases#
nhighestdayscases <-function(name){
  select <- covid.data$location == name
  covid.data.location <-subset(covid.data, subset = select)
  ndays <-0
  max<-0
  for(i in 1:nrow(covid.data.location))
  {
    if(!is.na(covid.data.location$new_cases[i])){
      max<-covid.data.location$new_cases[i]
      break
    }
  }
  for(i in 1:nrow(covid.data.location))
  {
    if(is.na(covid.data.location$new_cases[i])){
      next
    }
    if(max<covid.data.location$new_cases[i]){
      max <- covid.data.location$new_cases[i]
      ndays <- ndays +1
    }
  }
  print(ndays)
}
#Brazil
nhighestdayscases(name.country[1])
#Chile
nhighestdayscases(name.country[2])
#Venezuela
nhighestdayscases(name.country[3])

#new deaths#
nhighestdaysdeaths <-function(name){
  select <- covid.data$location == name
  covid.data.location <-subset(covid.data, subset = select)
  ndays <-0
  max<-0
  for(i in 1:nrow(covid.data.location))
  {
    if(!is.na(covid.data.location$new_deaths[i])){
      max<-covid.data.location$new_deaths[i]
      break
    }
  }
  for(i in 1:nrow(covid.data.location))
  {
    if(is.na(covid.data.location$new_deaths[i])){
      next
    }
    if(max<covid.data.location$new_deaths[i]){
      max <- covid.data.location$new_deaths[i]
      ndays <- ndays +1
    }
  }
  print(ndays)
}
#Brazil
nhighestdaysdeaths(name.country[1])
#Chile
nhighestdaysdeaths(name.country[2])
#Venezuela
nhighestdaysdeaths(name.country[3])





#############################################################
#4)Tables
#Missing value#
  covid.data.NA <-subset(covid.data, subset = (covid.data$location==name.country[1]|covid.data$location==name.country[2]|covid.data$location==name.country[3])
                                                    &(is.na(covid.data$new_cases)|is.na(covid.data$new_deaths)))
  covid.data.NA <- data.frame(covid.data.NA$location, covid.data.NA$new_cases, covid.data.NA$new_deaths)
  names(covid.data.NA) <- c("Countries", "Infections", "Deaths")
#new report#  
  covid.data.newreport <-subset(covid.data, subset = (covid.data$location==name.country[1]|covid.data$location==name.country[2]|covid.data$location==name.country[3])
                         &(!(is.na(covid.data$new_cases)|is.na(covid.data$new_deaths))))
  covid.data.newreport <- data.frame(covid.data.newreport$location, covid.data.newreport$new_cases, covid.data.newreport$new_deaths)
  names(covid.data.newreport) <- c("Countries", "Infections", "Deaths")

