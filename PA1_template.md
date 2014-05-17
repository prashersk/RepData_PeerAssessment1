# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

```r
setwd("/Users/satyendrakumar/RepData_PeerAssessment1")
unzip("./activity.zip", exdir = "activity_data")
activity <- read.csv("activity_data/activity.csv", header = TRUE, sep = ",")
```


## What is mean total number of steps taken per day?

```r
steps_pd <- tapply(activity$steps, activity$date, sum)
hist(steps_pd)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
mean(steps_pd, na.rm = T)
```

```
## [1] 10766
```

```r
median(steps_pd, na.rm = T)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
activity_pd <- tapply(activity$steps, activity$interval, sum, na.rm = TRUE)
plot(activity_pd, type = "l", xlab = "Interval", ylab = "Activity")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
which.max(activity_pd)
```

```
## 835 
## 104
```


## Imputing missing values

```r
total_NA <- length(which(is.na(activity)))
total_NA
```

```
## [1] 2304
```

```r

# strategy for filling NAs
for (i in which(sapply(activity, is.numeric))) {
    activity[is.na(activity[, i]), i] <- mean(activity[, i], na.rm = TRUE)
}

# New Dataset with NA filled.
activity_filled <- data.frame(activity)
head(activity_filled)
```

```
##   steps       date interval
## 1 37.38 2012-10-01        0
## 2 37.38 2012-10-01        5
## 3 37.38 2012-10-01       10
## 4 37.38 2012-10-01       15
## 5 37.38 2012-10-01       20
## 6 37.38 2012-10-01       25
```

```r

# Total number of Steps take
steps_pd_filled <- tapply(activity_filled$steps, activity_filled$date, sum, 
    na.rm = T)
hist(steps_pd_filled)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean_filled <- mean(steps_pd_filled)
mean_filled
```

```
## [1] 10766
```

```r
meadian_filled <- median(steps_pd_filled)
meadian_filled
```

```
## [1] 10766
```

```r
# There is no difference between mean but median is slightly changed
```


## Are there differences in activity patterns between weekdays and weekends?

```r
activity_filled$Date <- strptime(activity_filled$date, "%Y-%m-%d")
week_d <- as.character(weekdays(activity_filled$Date))
for (i in 1:length(week_d)) {
    if (week_d[i] == "Saturday" | week_d[i] == "Sunday") {
        week_d[i] <- "weekend"
    } else {
        week_d[i] <- "weekday"
    }
}
activity_filled$wd <- factor(week_d)
# Plotting the weekend and weekday activity
par(mfrow = c(2, 1))
for (type in c("weekend", "weekday")) {
    sub_set <- data.frame(subset(activity_filled, (activity_filled$wd == type)))
    Steps <- tapply(sub_set$steps, sub_set$interval, mean)
    plot(Steps, type = "l", main = type)
}
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

