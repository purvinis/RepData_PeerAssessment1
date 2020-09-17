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


activityWQtr <- activity %>% mutate(qtr = qday(date))

# 0s are omitted for the mean and medians (NAs replaced with 0s also).
# Because days with no activity recorded should not be included.
# What about intervals? Sometimes a person might be sitting....no steps
dailySums <- aggregate(steps ~ qtr, data = activityWQtr, FUN = sum, na.action = na.omit )
aveStepsPerDay <- mean(dailySums$steps)
medStepsPerDay <- median(dailySums$steps) 
hist(dailySums$steps,
     main = "Frequency of steps per day",
     xlab = "Steps in a day",
     col = rgb(0,1,1,.2))
  #   col = rainbow(5))

# alternate method to get data with no NAs 
#test <- activityWQtr[complete.cases(activityWQtr),]
#testday <-aggregate(stepsz ~ qtr, data = test, 
#                    FUN = "mean",
 #                   na.action = na.omit )

