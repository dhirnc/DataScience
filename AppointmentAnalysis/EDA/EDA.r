library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)
dataset <- read.csv("KaggleV2-May-2016.csv", stringsAsFactors = F)

nas <- sum(is.na(dataset))

blanks <- sum(dataset=="")

summary(dataset)


#Removing unnecessary columns
dataset$PatientId <- NULL
dataset$AppointmentID <- NULL
derived_ds <- dataset
derived_ds$AppointmentDay <- str_replace_all(derived_ds$AppointmentDay,"T"," ")
derived_ds$AppointmentDay <- str_replace_all(derived_ds$AppointmentDay,"Z","")
derived_ds$ScheduledDay <- str_replace_all(derived_ds$ScheduledDay,"T"," ")
derived_ds$ScheduledDay <- str_replace_all(derived_ds$ScheduledDay,"Z","")


derived_ds$AppointmentDay <- parse_date_time(derived_ds$AppointmentDay,orders = c("ymd HMS"),locale = "eng")

derived_ds$ScheduledDay <- parse_date_time(derived_ds$ScheduledDay,orders = c("ymd HMS"),locale = "eng")

derived_ds$App.WeekDay <- weekdays(derived_ds$AppointmentDay)

derived_ds$App.Day <- format(derived_ds$AppointmentDay,"%d")

derived_ds$Gap <- round(derived_ds$AppointmentDay - derived_ds$ScheduledDay,0)
derived_ds$Gap <- ifelse(derived_ds$Gap<0,0,derived_ds$Gap)
derived_ds$Gap <- round(derived_ds$Gap/24,1)

#Cleaning dirty rows

derived_ds$Age[which(derived_ds$Age==-1)] <- 0

#Factoring categorical variables

derived_ds$Gender <- as.factor(derived_ds$Gender)
derived_ds$Neighbourhood <- as.factor(derived_ds$Neighbourhood)
derived_ds$App.WeekDay <- as.factor(derived_ds$App.WeekDay)
derived_ds$App.Day <- as.factor(derived_ds$App.Day)

#Binning Age group


derived_ds$No.show <- ifelse(derived_ds$No.show=="Yes",1,0)
derived_ds$Age.Group <- ''

st<-timestamp()
for(i in 1:nrow(derived_ds)){
  
  if(!is.na(derived_ds$Age[i])){
    if(derived_ds$Age[i] <= 1){
      derived_ds$Age.Group[i] <- 'Infant'
    } else if(derived_ds$Age[i] > 1 & derived_ds$Age[i] <=3 &(!is.na(derived_ds$Age[i]))){
      derived_ds$Age.Group[i] <- 'Toddler'
    } else if(derived_ds$Age[i] > 3 & derived_ds$Age[i] <=12 &(!is.na(derived_ds$Age[i]))){
      derived_ds$Age.Group[i] <- 'Pre School'
    } else if(derived_ds$Age[i] > 12 & derived_ds$Age[i] <= 17&(!is.na(derived_ds$Age[i]))){
      derived_ds$Age.Group[i] <- 'Grad School'
    } else if(derived_ds$Age[i] > 17 & derived_ds$Age[i] <= 21&(!is.na(derived_ds$Age[i]))){
      derived_ds$Age.Group[i] <- 'Young adult'
    } else if(derived_ds$Age[i] > 21 & derived_ds$Age[i] <= 40&(!is.na(derived_ds$Age[i]))){
      derived_ds$Age.Group[i] <- 'Adult'
    } else if(derived_ds$Age[i] > 40 & derived_ds$Age[i] <= 60&(!is.na(derived_ds$Age[i]))){
      derived_ds$Age.Group[i] <- 'Senior Citizen'
    } else if(derived_ds$Age[i] > 60){ 
      derived_ds$Age.Group[i] <- 'Old Citizen' 
    }
  } else{
    
  }
}
et<-timestamp()-st

#Univariate Analysis
ggplot(derived_ds,aes(x=as.factor(No.show),fill=as.factor(No.show))) + geom_bar()


ggplot(derived_ds,aes(x=Age,fill=as.factor(No.show))) + geom_histogram(binwidth = 10)




ggplot(derived_ds,aes(x=as.factor(Age.Group),fill=as.factor(No.show))) + geom_bar() #+ facet_wrap(~Gender) + 
