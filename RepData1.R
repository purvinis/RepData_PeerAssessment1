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

teesttime <-strptime( paste(activityWQtr$interval[4]%/%100,".",activityWQtr$interval[4] %% 100),
                      format = "%H . %M")


# 0s should stay in data.
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
#------------------------------------------------------------------------------
#hat is the average daily activity pattern?
 # Make a time series plot (i.e.type = "l")
#of the 5-minute interval (x-axis) and the average number of steps taken, 
#averaged across all days (y-axis)
#Which 5-minute interval, on average across all the days in the dataset, 
#contains the maximum number of steps?
#
#A ts plot needs time on the axis. Convert interval to time/date format.
# There are 288 5-minute intervals in 24 hours. The interval id 

intervalAves <- aggregate(steps ~ interval , data = activityWQtr, 
                          FUN = mean, na.action = na.omit )
t<-data.frame()
for (m in seq(1,288,by = 24)){
  ti <- c(paste0(intervalAves$interval[m]%/%100,":",intervalAves$interval[m] %% 100),
          intervalAves$interval[m])
  t <- rbind(t,ti)
}
colnames(t) <- c("hr_min","interval")

p2 <- ggplot(intervalAves, aes(interval,steps))+
  geom_line()+
  xlab("interval, minutes")+
  ylab("Average steps")+
  scale_x_continuous("time, hourly",breaks = seq(1,2355,by = 200),labels = t$hr_min)+
  labs(title = "Average steps per 5 minute interval")

print(p2)

#Which interval contains max number of steps?
intervalMaxIndex <- which.max(activityWQtr$steps )  #16492
mostSteps <- activityWQtr$step[intervalMaxIndex]    #806
intervalMax <- activityWQtr$interval[intervalMaxIndex]  #615
intervalMaxTime <-paste0(intervalMax%/%100,":",
                          intervalMax%% 100)
  
#check to see if this is true, since it is not the time with highest average
plot(activityWQtr$interval,activityWQtr$step)

#-------------------------------------------------------------------------------
#Impute missing values
#Make a histogram of the total number of steps taken each day and Calculate and 
#report the mean and median total number of steps taken per day. Do these values 
#differ from the estimates from the first part of the assignment? What is the 
#impact of imputing missing data on the estimates of the total daily number of steps?

#Calculate number of NAs:
NAsTot <-sum(is.na(activityWQtr$steps))  #could also use summary()

#Replace NAs with mean of the interval. 

uniqueDays <- unique(activityWQtr$qtr)  #list of 61
uniqueIntervals <-unique(activityWQtr$interval) #list of 288
activityNoNA <-data.frame()
for (i in 1:288) {
  NoNAdata <- activityWQtr %>% subset(interval == interval[i]) %>%
    mutate(newSteps = replace_na(steps,intervalAves$steps[i]))
  activityNoNA <-rbind(activityNoNA,NoNAdata)
}

#New histogram of total number of steps each day.
dailySumsNoNA <- aggregate(newSteps ~ qtr, data = activityNoNA, FUN = sum )
aveStepsPerDayNoNA <- mean(dailySumsNoNA$newSteps)
medStepsPerDayNoNA <- median(dailySumsNoNA$newSteps) 
hist(dailySumsNoNA$newSteps,
     main = "Frequency of steps per day",
     xlab = "Steps in a day",
     col = rgb(1,0,1,.2))
