# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```r
library(ggplot2)
unzip(file.path("activity.zip"), exdir = file.path("data"))
data <- read.csv(file.path("data", "activity.csv"))
```


## What is mean total number of steps taken per day?
####1. Calculate the total number of steps taken per day

```r
totalSteps <- aggregate(steps ~ date, data = data, sum)
```

####2. Make a histogram of the total number of steps taken each day

```r
ggplot(data = totalSteps) + geom_histogram(aes(x = steps, fill = ..count..), binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

####3. Calculate and report the mean and median of the total number of steps taken per day
Mean total steps taken per day:

```r
mean(totalSteps$steps)
```

```
## [1] 10766.19
```

Median total steps taken per day:

```r
median(totalSteps$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
#### 1. Calculate daily average per interval

```r
intervalAverage <- aggregate(steps ~ interval, data = data, mean)
```

#### 2. Make a time series plot of the 5-minute interval and the average number of steps taken

```r
qplot(x = interval, y = steps, data = intervalAverage, geom = "line") 
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

#### 3. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
intervalAverage$interval[which.max(intervalAverage$steps)]
```

```
## [1] 835
```


## Imputing missing values
#### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(!complete.cases(data))
```

```
## [1] 2304
```

#### 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
Replace with the average 5-minute interval value

#### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
dataCopy <- data
for (i in 1:nrow(dataCopy)) {
  if (is.na(dataCopy[i,1])) {
    dataCopy[i,1] <- intervalAverage$steps[intervalAverage$interval == dataCopy[i,3]]
  }
}
head(dataCopy)
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

#### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
imputedTotalSteps <- aggregate(steps ~ date, data = dataCopy, sum)
ggplot(data = imputedTotalSteps) + geom_histogram(aes(x = steps, fill = ..count..), binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 
Mean total steps taken per day:

```r
mean(imputedTotalSteps$steps)
```

```
## [1] 10766.19
```

Median total steps taken per day:

```r
median(imputedTotalSteps$steps)
```

```
## [1] 10766.19
```

Comparing the values computed from the imputed data it looks as though the mean stays the same but the median gets skewed a little higher and shows a little weirdness with it being the exact same as the mean. Looks as though there is more frequency in the 10,000-10,999 bin as well.

## Are there differences in activity patterns between weekdays and weekends?
#### 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
dataCopy <- transform(dataCopy, dayType = as.factor(ifelse(weekdays(as.Date(dataCopy$date)) %in% c("Saturday","Sunday"), "Weekend", "Weekday")))
head(dataCopy)
```

```
##       steps       date interval dayType
## 1 1.7169811 2012-10-01        0 Weekday
## 2 0.3396226 2012-10-01        5 Weekday
## 3 0.1320755 2012-10-01       10 Weekday
## 4 0.1509434 2012-10-01       15 Weekday
## 5 0.0754717 2012-10-01       20 Weekday
## 6 2.0943396 2012-10-01       25 Weekday
```

#### 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```r
imputedIntervalAverage <- aggregate(steps ~ interval + dayType, data = dataCopy, mean)
qplot(x = interval, y = steps, data = imputedIntervalAverage, facets = dayType ~ ., geom = "line")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png) 
