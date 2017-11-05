library(dplyr)
library(tidyr)
library(lubridate)
dataset <- read.csv("KaggleV2-May-2016.csv", stringsAsFactors = F)

nas <- sum(is.na(dataset))

blanks <- sum(dataset=="")

summary(dataset)


#Removing unnecessary columns
dataset$PatientId <- NULL
dataset$AppointmentID <- NULL

derived_ds <- separate(dataset, ScheduledDay, into=c("Sch.Date", "Sch.Time"), sep="T")
derived_ds <- separate(derived_ds, AppointmentDay, into=c("App.Date","App.Time"),sep="T")
derived_ds$App.Time <- NULL
derived_ds <- separate(derived_ds, Sch.Time, into=c("Sch.Time"),sep="Z")


derived_ds$Sch.Date <- parse_date_time(derived_ds$Sch.Date,orders = c("ymd"),locale = "eng")
derived_ds$Sch.Time <- parse_date_time(derived_ds$Sch.Time,orders=c("HMS"),locale = "eng")

derived_ds$App.Date <- parse_date_time(derived_ds$App.Date,orders=c("ymd"),locale = "eng")

derived_ds$Sch.WeekDay <- weekdays(derived_ds$Sch.Date)
derived_ds$Sch.Hour <- format(derived_ds$Sch.Time,"%H")
derived_ds$Sch.Day <- format(derived_ds$Sch.Date,"%d")



#Factoring categorical variables

derived_ds$Gender <- as.factor(derived_ds$Gender)
derived_ds$Neighbourhood <- as.factor(derived_ds$Neighbourhood)
derived_ds$Sch.WeekDay <- as.factor(derived_ds$Sch.WeekDay)
derived_ds$Sch.Day <- as.factor(derived_ds$Sch.Day)
derived_ds$Sch.Hour <- as.factor(derived_ds$Sch.Hour)



derived_ds$No.show <- ifelse(derived_ds$No.show=="Yes",1,0)



#Univariate Analysis

