# Author:  Robert McIlvride
# Platform:  x86_64-apple-darwin13.4.0
# Environment:  R Version 3.1.2, RStudio Version 0.98.1102
# Project:  RepRes_Peer_Assessment1.Rproj
# Program:  pa1.R

#
# Description:
#
# It is now possible to collect a large amount of data about personal movement
# using activity monitoring devices such as a Fitbit (http://www.fitbit.com),
# Nike Fuelband (http://www.nike.com/us/en_us/c/nikeplus- fuelband), or Jawbone
# Up (https://jawbone.com/up).
#
# These type of devices are part of the “quantified self” movement – a group of
# enthusiasts who take measurements about themselves regularly to improve their
# health, to find patterns in their behavior, or because they are tech geeks.
# But these data remain under- utilized both because the raw data are hard to
# obtain and there is a lack of statistical methods and software for processing
# and interpreting the data.
#
# This assignment makes use of data from a personal activity monitoring device.
# This device collects data at 5 minute intervals through out the day. The data
# consists of two months of data from an anonymous individual collected during
# the months of October and November, 2012 and include the number of steps
# taken in 5 minute intervals each day.

#
# Datasets:
#
# The data for this assignment can be downloaded from the course web site:
#     Dataset: Activity monitoring data
#     (https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip)
#
# The variables included in this dataset are:
#
#   steps: Number of steps taken in a 5-min interval (missing vals coded NA)##
#
#   date: The date on which the measurement was taken in YYYY-MM-DD format
#
#   interval: Identifier for the 5-min interval in which measurement was taken
#
# The dataset is stored in a comma-separated-value (CSV) file and there are a
# total of 17,568 observations in this dataset.
#

# Clear the R environment
rm(list=ls(all=TRUE))

# Prevent scientific notation on the plot
options(scipen=10)

# Required libraries
library(ggplot2)
library(scales)
library(date)

##
## Load and pre-process the data
##

# Read dataset
if(!file.exists('./raw_data/')){dir.create('./raw_data/')}
fileUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
temp <- tempfile()
download.file(fileUrl, temp, method = 'curl')
unzip(temp, exdir = './raw_data')
unlink(temp)

df.activity <- read.csv("./raw_data/activity.csv")

# Pre-process data

#df.activity$date <- as.Date(df.activity$date,format="%Y/%m/%d")
#df.activity$date<-as.POSIXct(df.activity$date,format="%Y/%m/%d")

# replace cryptic abbreviations with more descriptive words
# df.features[2,] <- gsub("^t","time_",df.features[2,],ignore.case=T)
# df.features[2,] <- gsub("^f","freq_",df.features[2,],ignore.case=T)
# df.features[2,] <- gsub("-","_",df.features[2,],ignore.case=T)
# df.features[2,] <- gsub("[^A-Za-z0-9_]","",df.features[2,],ignore.case=T)
# df.features[2,] <- gsub("bodybody","Body",df.features[2,],ignore.case=T)
# df.features[2,] <- gsub("^anglet","angle_time_",df.features[2,],ignore.case=T)
# df.features[2,] <- gsub("^anglex","angle_X_",df.features[2,],ignore.case=T)
# df.features[2,] <- gsub("^angley","angle_Y_",df.features[2,],ignore.case=T)
# df.features[2,] <- gsub("^anglez","angle_Z_",df.features[2,],ignore.case=T)

##
## Analyze data
##

# Determine the mean total number of steps taken per day
#
# For this part of the assignment, you can ignore the missing values in the
# dataset. Calculate the total number of steps taken per day. If you do not
# understand the difference between a histogram and a barplot, research the
# difference between them.
#
# Make a histogram of the total number of steps taken each day and calculate and
# report the mean and median total number of steps taken per day. Do these
# values differ from the estimates from the first part of the assignment? What
# is the impact of imputing missing data on the estimates of the total daily
# number of steps?

# steps: Number of steps taken in a 5-min interval (missing vals coded NA)
# date: The date on which the measurement was taken in YYYY-MM-DD format
# interval: Identifier for the 5-min interval in which measurement was taken

df.activity.omit <- na.omit(df.activity)

png(filename = "plot1.png",
             width = 960, height = 480, units = "px",
             pointsize = 12, bg = "white")

df.daily_activity <- aggregate(steps ~ date, df.activity.omit, sum)

plot1 <- ggplot(df.daily_activity, aes(date)) +
                geom_histogram(aes(weight = steps, fill = ..count..),
                               color="white", binwidth = 1) +
                xlab("Date") + ylab("Steps Per Day") +
                ggtitle("Steps Taken Per Day") +
                theme(axis.text.x = element_text(angle = 90, hjust = 1))

print(plot1)
dev.off()

# Mean number of steps per day:
mean_daily_steps <- mean(df.daily_activity$steps)

# Median number of steps per day:
median_daily_steps <- median(df.daily_activity$steps)

# Determine the average daily activity pattern
#
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?

df.interval_mean_steps <- aggregate(steps ~ interval, df.activity.omit, mean)

# create dataframe with mean steps per interval
#df.mean.interval <- aggregate(df$steps, by=list(df$interval), mean, na.rm=TRUE, na.action=NULL)
#names(df.mean.interval)[1] <-"interval"
#names(df.mean.interval)[2] <-"mean.steps"

#
# Impute missing values
#

# Note that there are a number of days/intervals where there are missing values
# (coded as NA). The presence of missing days may introduce bias into some
# calculations or summaries of the data.
#
# Calculate and report the total number of missing values in the dataset (i.e.
# the total number of rows with NAs) Devise a strategy for filling in all of
# the missing values in the dataset.
#
# The strategy does not need to be sophisticated. For example, you could use
# the mean/median for that day, or the mean for that 5-minute interval, etc.
# Create a new dataset that is equal to the original dataset but with the
# missing data filled in.

# Testing for Missing Values
# is.na(x) # returns TRUE of x is missing
# y <- c(1,2,3,NA)
# is.na(y) # returns a vector (F F F T)

# Recoding Values to Missing
# recode 99 to missing for variable v1
# select rows where v1 is 99 and recode column v1
# mydata$v1[mydata$v1==99] <- NA

## Analyze differences in activity patterns between weekdays and weekends
##
## For this part the weekdays() function may be of some help here. Use the
## dataset with the filled-in missing values for this part. Create a new factor
## variable in the dataset with two levels – “weekday” and “weekend” indicating
## whether a given date is a weekday or weekend day.
##
## Make a panel plot containing a time series plot (i.e. type = "l") of the
## 5-minute interval (x-axis) and the average number of steps taken, averaged
## across all weekday days or weekend days (y-axis). See the README file in the
## GitHub repository to see an example of what this plot should look like using
## simulated data.



## End of program