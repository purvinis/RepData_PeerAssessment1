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
names (activity)
str(activity)
activity$date <- date(activity$date)  #convert to class Date
summary(activity)

activityWQtr <- activity %>% mutate(qtr = qday(date))  #add a column for unique date in quarter
dailyMeans <- aggregate(steps ~ qtr, data = activityWQtr, FUN = mean, na.action = na.pass )
hist(activity$steps)
