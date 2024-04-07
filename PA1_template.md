---
title: "Reproducible Research Project 1"
author: "Dyutit Mohanty"
date: "2024-03-11"
output: html_document
---

# Loading and preprocessing the data


```r
filename <- "activity.csv"
data <- read.csv(filename, header=TRUE)
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

# What is mean total number of steps taken per day?

### Calculate the total number of steps taken per day

```r
data2 <- data[!(is.na(data$steps)), ]
steps_per_day <- aggregate(steps ~ date, data=data2, FUN = sum)
```

### Histogram of the total number of steps taken each day

```r
hist(steps_per_day$steps, main="Histogram of the total number of steps taken each day", xlab="Steps", ylab="Frequency")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

### Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps_per_day <- mean(steps_per_day$steps)
median_steps_per_day <- median(steps_per_day$steps)
mean_steps_per_day
```

```
## [1] 10766.19
```

```r
median_steps_per_day
```

```
## [1] 10765
```


# What is the average daily activity pattern?

### Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
my_mean <- function(x) mean(x, na.rm = TRUE) # R kept throwing an error when "mean" was used as arg so I defined my own mean function
data3 <- aggregate(steps ~ interval, data, FUN=my_mean)
head(data3)
```

```
##   interval     steps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```


```r
plot(y=data3$steps,x=data3$interval, type="l", xlab="Intervals", ylab="Steps")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
index_max_steps <- which.max(data3$steps)
interval_max_steps <- data3$interval[index_max_steps]
interval_max_steps
```

```
## [1] 835
```

```r
max(data3$steps)
```

```
## [1] 206.1698
```


# Imputing missing values

### Calculate and report the total number of missing values in the dataset (total number of rows with NAs)

```r
data4 <- subset(data, is.na(data$steps))
dims <- dim(data4)
nrows_na <- dims[1]
nrows_na
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
# I am using the mean for the interval
data_nona <- data

for (i in 1:nrow(data_nona)){
  if (is.na(data_nona$steps[i])){
    interval <- data_nona$interval[i]
    index <- which(data3$interval == interval)
    mean_val <- data3$steps[index]
    data_nona$steps[i] <- mean_val
  }
}

head(data_nona)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```


### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
head(data_nona)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
data4 <- aggregate(steps ~ date, data_nona, FUN=sum)
hist(data4$steps, main="Histogram of the total number of steps taken each day", xlab="Steps", ylab="Frequency")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

```r
mean_steps <- mean(data4$steps)
median_steps <- median(data4$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10766.19
```
### Slight increase in average


# Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
data5 <- data_nona
data5$date <- as.Date(data5$date)
data5$day <- weekdays(data5$date)

for (i in 1:nrow(data5)){
  if (data5$day[i] %in% c("Saturday", "Sunday")){
    data5$weekday[i] <- "weekend"
  }
  else {
    data5$weekday[i] <- "weekday"
  }
}

fct_days <- factor(data5$weekday)
summary(fct_days)
```

```
## weekday weekend 
##   12960    4608
```

### Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
wknd_data <- data5[data5$weekday == "weekend",]
wkday_data <- data5[data5$weekday == "weekday",]

wknd_data <- aggregate(steps ~ interval, wknd_data, mean)
```

```
## Error in get(as.character(FUN), mode = "function", envir = envir): object 'FUN' of mode 'function' was not found
```

```r
wkday_data <- aggregate(steps ~ interval, wkday_data, mean)
```

```
## Error in get(as.character(FUN), mode = "function", envir = envir): object 'FUN' of mode 'function' was not found
```

```r
par(mfrow=c(2, 1))
plot(wknd_data$interval, wknd_data$steps, type="l")
plot(wkday_data$interval, wkday_data$steps, type="l")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

