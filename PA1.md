# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


<--- #echo=FALSE, results='asis'}
--->


```r
if(!file.exists('./raw_data/')){dir.create('./raw_data/')}
fileUrl <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
temp <- tempfile()
download.file(fileUrl, temp, method = 'curl')
unzip(temp, exdir = './raw_data')
unlink(temp)

df.activity <- read.csv("./raw_data/activity.csv")

summary(df.activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

knitr::kable(head(mtcars, 10))


## What is mean total number of steps taken per day?



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
