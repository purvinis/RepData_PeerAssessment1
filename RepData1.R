#######################################################################
## Programmer: GP
## Date: 9/15/2020
## Project: ReproData_PeerAssignment1.RProj
## Description: Reprodcible Research Week 2 Project 1

#######################################################################

# Load needed packages and download the data
library(dplyr)
library(tidyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(stats)
# dataset: Get zipped file from course website and put in current director
# Then unzip it

zipfile <- dir()[grep("zip",dir())]             #get the zip file name
unzip(zipfile)                            #unzip in current directory

# One file will result with an *.csv extension
# Use dir() to see the names

#read data into R and look at contents
activity <- read.csv("activity.csv",sep = ",")

activity$date <- date(activity$date)  #convert to class Date


activityWQtr <- activity %>% mutate(qtr = qday(date))%>%
    mutate(stepsz = na_if(steps,0))
dailyMeans <- aggregate(stepsz ~ qtr, data = activityWQtr, 
                        FUN = "mean",
                        na.action = na.omit )

# returns 89 for the mean for day 47 (to check)
test <-  activityWQtr %>% filter(qtr == 47) %>% filter(!is.na(stepsz)) %>%
select(stepsz) %>% apply(2,FUN =mean)

dailySums <- aggregate(steps ~ qtr, data = activityWQtr, FUN = sum, na.action = na.pass )
dailyMedians <- aggregate(stepsz ~ qtr, data = activityWQtr, FUN = median, na.action = na.pass )
aveStepsPerDay <- mean(dailyMeans$stepsz,na.rm = TRUE)
medStepsPerDay <- median(dailyMeans$steps[dailyMeans$steps],na.rm = TRUE)
hist(dailySums$steps,
     main = "Frequency of steps per day",
     xlab = "Steps in a day",
  #   col = rgb(0,1,1,.2))
     col = rainbow(5))
 

