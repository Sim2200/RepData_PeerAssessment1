---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
data <- read.csv("activity.csv")
```


## What is mean total number of steps taken per day?

```r
library(ggplot2)
library(dplyr)
totalsteps <- activity %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 
hist(totalsteps$sumsteps, main = "Histogram of Total Steps Per Day", 
     col="gray", xlab="Total Steps", ylab="Count", ylim = c(0,50))
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png)

```r
mean <- round(mean(totalsteps$sumsteps))
median <- round(median(totalsteps$sumsteps))

print(paste("The mean is: ", mean))
```

```
## [1] "The mean is:  9354"
```

```r
print(paste("The median is: ", median))
```

```
## [1] "The median is:  10395"
```


## What is the average daily activity pattern?



```r
stepsbytime <- activity %>%
        group_by(interval) %>%
        summarize(meansteps = mean(steps, na.rm = TRUE))

plot(stepsbytime$meansteps ~ stepsbytime$interval,
     col="black", type="l", xlab = "5-Minute Intervals", ylab = "Average Number of Steps",
     main = "Average Daily Activity Pattern")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

```r
print(paste("5-Minute Interval containing the maximum number of steps on average across all days is: ",stepsbytime$interval[which.max(stepsbytime$meansteps)]))
```

```
## [1] "5-Minute Interval containing the maximum number of steps on average across all days is:  835"
```

```r
print(paste("Average steps for that interval: ",round(max(stepsbytime$meansteps))))
```

```
## [1] "Average steps for that interval:  206"
```


## Imputing missing values


There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.


```r
print(paste("The total number of rows with missing values in the dataset is: ",sum(is.na(activity$steps))))
```

```
## [1] "The total number of rows with missing values in the dataset is:  2304"
```

All of the missing values are filled in with average value for that 5-minute
interval.


```r
activityNoNA <- activity  
for (i in 1:nrow(activity)){
        if(is.na(activity$steps[i])){
                activityNoNA$steps[i]<- stepsbytime$meansteps[activityNoNA$interval[i] == stepsbytime$interval]
        }
}
totalsteps <- activityNoNA %>%
        group_by(date) %>%
        summarize(sumsteps = sum(steps, na.rm = TRUE)) 

hist(totalsteps$sumsteps, main = "Histogram of Steps taken each day", 
     col="gray", xlab="Steps", ylab="Count")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

```r
meanNA <- round(mean(totalsteps$sumsteps), digits = 2)
medianNA <- round(median(totalsteps$sumsteps), digits = 2)

print(paste("The mean is: ", mean(meanNA)))
```

```
## [1] "The mean is:  10766.19"
```

```r
print(paste("The median is: ", median(medianNA)))
```

```
## [1] "The median is:  10766.19"
```

```r
NACompare <- data.frame(mean = c(mean,meanNA),median = c(median,medianNA))
rownames(NACompare) <- c("Pre NA values", "Post NA values")
print(NACompare)
```

```
##                    mean   median
## Pre NA values   9354.00 10395.00
## Post NA values 10766.19 10766.19
```

Mean and median values are higher after including the missing data. the mean increases from 9354 to 10766, and the median increases from 10395 to 10766. This is because in the original data, there are some days with `steps` with values `NA` for 
any `interval`. The total number of steps taken in such days are set to 0 by
default. After replacing missing `steps` values with the mean `steps`
of associated `interval` value, these 0 values are removed from the histogram
of total number of steps taken each day.


## Are there differences in activity patterns between weekdays and weekends?


First, let's find the day of the week for each measurement in the dataset. In
this part, we use the dataset with the filled-in values.


```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
activitylevels$date <- as.Date(activitylevels$date)
activitylevels$day <- sapply(activitylevels$date, FUN=weekday.or.weekend)
```

Now, let's make a panel plot containing plots of average number of steps taken
on weekdays and weekends.

```r
averages <- aggregate(steps ~ interval + day, data=activitylevels, FUN="mean")
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

