# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Activity data set contains 3 variables; Steps, Date and Interval. This data was collected for October and November of 2012.


```r
activityOrig <- read.csv("activity.csv", stringsAsFactors = FALSE)

## Transform date column to actual date format
activityOrig$date <- as.POSIXct(activityOrig$date, format="%Y-%m-%d")

## Add day of the week to the data set and convert it to data frame
activityOrig <- data.frame(date=activityOrig$date,
                           weekday=tolower(weekdays(activityOrig$date)),
                           steps=activityOrig$steps,
                           interval=activityOrig$interval)

## Mark days as week day or weekday for computation and create final data frame.
activityDF <- cbind(activityOrig,
                   daytype=ifelse(activityOrig$weekday == "saturday" | activityOrig$weekday == "sunday", 
                                  "weekend",
                                  "weekday"))

## Rearrange columns in the data frame
activityDF <- data.frame(date=activityDF$date,
                         weekday=activityDF$weekday,
                         daytype=activityDF$daytype,
                         interval=activityDF$interval,
                         steps=activityDF$steps)

## Clear the workspace
rm(activityOrig)

## Show few rows from final data set.
head(activityDF)
```

```
##         date weekday daytype interval steps
## 1 2012-10-01  monday weekday        0    NA
## 2 2012-10-01  monday weekday        5    NA
## 3 2012-10-01  monday weekday       10    NA
## 4 2012-10-01  monday weekday       15    NA
## 5 2012-10-01  monday weekday       20    NA
## 6 2012-10-01  monday weekday       25    NA
```



## 1. What is mean total number of steps taken per day?
###1.1. Compute total number of steps taken per day
    For this part of the assignment missing values in the dataset can be ignored.

```r
stepsByDay <- aggregate(activityDF$steps, by=list(activityDF$date), FUN=sum, na.rm=TRUE)

## Name the columns in the data set.
names(stepsByDay) <- c("day", "steps")

## Show few rows from the data set
head(stepsByDay)
```

```
##          day steps
## 1 2012-10-01     0
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

###1.2. Draw histogram

```r
## Draw histogram of total number of steps taken per day.
hist(stepsByDay$steps,
     breaks=seq(from=0, to=25000, by=2500),
     col="green",
     xlab="Total Steps",
     ylim=c(0,20),
     main="Histogram of Total Number of Steps Taken Per Day\n (NA Removed)")
```

![](PA1_template_files/figure-html/question1.2-1.png) 

### 1.3 Calculate and report the mean and median of the total number of steps taken per day.

```r
lMean <- round(mean(stepsByDay$steps), digits=0)
lMedian <- median(stepsByDay$steps)
```
Mean and median of total steps taken per day are **9354** and **10395** respectively.

## 2. What is the average daily activity pattern?
### 2.1 Make a time series plot of the 5 minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis).


```r
## Clear workspace
rm(stepsByDay)

## Calculate mean steps by interval
meanStepsByInterval <- aggregate(activityDF$steps,
                                 by=list(activityDF$interval),
                                 FUN=mean,
                                 na.rm=TRUE)
names(meanStepsByInterval) <- c("interval", "mean")

head(meanStepsByInterval)
```

```
##   interval      mean
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
## Create time series plot
plot(meanStepsByInterval$interval,
     meanStepsByInterval$mean,
     type="l",
     col="green",
     lwd=2,
     xlab="Interval [minutes]",
     ylab="Average Number of Steps",
     main="Time Series Plot of the Average Numbers of Steps Taken per Interval\n (NA Removed)")
```

![](PA1_template_files/figure-html/question2.1-1.png) 

### 2.2 Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
## Find the position of maximum mean in the data set
highestMeanPos <- which(meanStepsByInterval$mean == max(meanStepsByInterval$mean))
peakInterval <- meanStepsByInterval[highestMeanPos, 1]
```

The 5-minute interval which contains the maximum number of steps, on an average across all days, is **835**.

## 3. Imputing missing values
###3.1 Calculate and report the total number of missing values in the dataset

```r
## Take a sum of the resuts of is.na on the data set.
naCount <- sum(is.na(activityDF$steps))
```

Total number of missing values (NA) is **2304**.

###3.2 Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
## Find the positions where NA is
naPosition <- which(is.na(activityDF$steps))

## Find the mean of steps taken across the data set and fill in the NA values with the mean.
naVector <- rep(mean(activityDF$steps, na.rm=TRUE), times=length(naPosition))
```

###3.3 Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
## Replace NA vaue with the mean value in all positions.
activityDF[naPosition, "steps"] <- naVector

## Show few rows from the modified data set.
head(activityDF)
```

```
##         date weekday daytype interval   steps
## 1 2012-10-01  monday weekday        0 37.3826
## 2 2012-10-01  monday weekday        5 37.3826
## 3 2012-10-01  monday weekday       10 37.3826
## 4 2012-10-01  monday weekday       15 37.3826
## 5 2012-10-01  monday weekday       20 37.3826
## 6 2012-10-01  monday weekday       25 37.3826
```

###3.4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsByDay <- aggregate(activityDF$steps, by=list(activityDF$date), FUN=sum, na.rm=TRUE)

## Name the columns in the data set.
names(stepsByDay) <- c("day", "steps")

## Show few rows from the data set
head(stepsByDay)
```

```
##          day    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

```r
## Draw histogram of total number of steps taken per day.
hist(stepsByDay$steps,
     breaks=seq(from=0, to=25000, by=2500),
     col="blue",
     xlab="Total Steps",
     ylim=c(0,20),
     main="Histogram of Total Number of Steps Taken Per Day\n (NA Imputed)")
```

![](PA1_template_files/figure-html/question3.4-1.png) 

```r
## Supress scientific notation
options("scipen"=100, "digits"=4)
lMean <- mean(stepsByDay$steps)
lMedian <- median(stepsByDay$steps)
```

Mean and median of total steps taken per day are **10766.1887** and **10766.1887** respectively.

New values varries greatly from the first part of the assignment as after imputed values the valid data set is larger than original (with all values populated). It is assumed that the subject has done activities while he may not have actually done any activity.

## 4. Are there differences in activity patterns between weekdays and weekends?
### 4.1 Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
## I have already created two variables weekday and datetype in the activity data set when raw data was initially processed.
head(activityDF)
```

```
##         date weekday daytype interval steps
## 1 2012-10-01  monday weekday        0 37.38
## 2 2012-10-01  monday weekday        5 37.38
## 3 2012-10-01  monday weekday       10 37.38
## 4 2012-10-01  monday weekday       15 37.38
## 5 2012-10-01  monday weekday       20 37.38
## 6 2012-10-01  monday weekday       25 37.38
```

### 4.2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
## Calculate aberage number of steps taken across daytype, weekday and interval
stepsByDaytype <- aggregate(activityDF$steps,
                            by=list(activityDF$daytype, activityDF$weekday, activityDF$interval),
                            mean)
## Rename the columns
names(stepsByDaytype) <- c("daytype", "weekday", "interval", "steps")

## Show few rows
head(stepsByDaytype)
```

```
##   daytype  weekday interval steps
## 1 weekday   friday        0 8.307
## 2 weekday   monday        0 9.418
## 3 weekend saturday        0 4.673
## 4 weekend   sunday        0 4.673
## 5 weekday thursday        0 9.376
## 6 weekday  tuesday        0 0.000
```

```r
library(lattice)
xyplot(steps ~ interval | daytype, stepsByDaytype,
       type = "l",
       lwd = 1,
       xlab = "Interval",
       ylab = "Number of Steps",
       layout = c(1,2))
```

![](PA1_template_files/figure-html/question4.2-1.png) 
