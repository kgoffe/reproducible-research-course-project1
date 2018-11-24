---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  word_document: default
  pdf_document: default
---
### Set up global option 

```r
knitr::opts_chunk$set(echo=TRUE,warning=FALSE, message=FALSE,comment=NA, fig.width=5, fig.height=5,cache=FALSE)

options(scipen = 1, digits = 2)
```

### Loading and preprocessing the data

```r
library(tidyr)
library(dplyr)
library(ggplot2)
library(mice)
setwd("~/Reproducible Research/activity")
#setwd("~/Coursera/RR")
activity <- read.csv("activity.csv", header = TRUE, sep=",")
activity_sub <- activity %>% drop_na()
```


### What is mean total number of steps taken per day?
1. Calculate the total number of steps taken per day (ignore NA)

```r
number_steps_per_day <- activity_sub %>% group_by(date) %>% 
        summarise(total_steps=sum(steps))
head(number_steps_per_day)
```

```
# A tibble: 6 x 2
  date       total_steps
  <fct>            <int>
1 2012-10-02         126
2 2012-10-03       11352
3 2012-10-04       12116
4 2012-10-05       13294
5 2012-10-06       15420
6 2012-10-07       11015
```
2.Make a histogram of the total number of steps takend each day

```r
hist(number_steps_per_day$total_steps, main="Number of steps taken per day",
     xlab="total number of steps", border="blue", col="green",breaks=6)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day

```r
mean_steps_per_day <- mean(number_steps_per_day$total_steps)
median_steps_per_day <- median(number_steps_per_day$total_steps)
```
        
+ Mean steps per day is 10766.19 steps
+ Median steps per day is 10765 steps

### What is the average daily activity pattern?
1. Time series plot of the 5 minute interval (x-axis) and average number of steps
taken, averaged across all days (y-axis)

```r
five_min_interval <- activity_sub %>% group_by(interval) %>% summarise(mean_steps=mean(steps))
ggplot(data=five_min_interval,aes(x=interval,y=mean_steps)) + geom_line() + 
        ggtitle("Average number of steps taken by interval") + 
        xlab("Interval") + ylab("Average number of steps taken across all day (average) ") + theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

2. Which 5-minute interval, on average accross all the days in the dataset, contains
the maximum number of steps?

```r
max <- five_min_interval[which.max(five_min_interval$mean_steps),]
```

835 interval has maximum steps of 206.17 

### Imputing missing values
1. Calculate and report the total number of missing values in the dataset

```r
total_NAs <- sum(!complete.cases(activity))
```

Total missing value in the dataset is 2304

2 & 3. Impute missing values

```r
completedata <- activity %>% 
        group_by(interval) %>% 
        mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```

4.Histogram of the total number of steps taken per day and report mean and median

```r
number_steps_per_day_impute <- completedata %>% group_by(date) %>% 
        summarise(total_steps=sum(steps))
hist(number_steps_per_day_impute$total_steps, 
     main="Number of steps taken per day (NA imputed)",
     xlab="total number of steps", border="purple", col="yellow",breaks=6)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
# Mean and Median value from the imputed value
mean_steps_per_day_impute <- mean(number_steps_per_day_impute$total_steps)
median_steps_per_day_impute <- median(number_steps_per_day_impute$total_steps)
```

new Mean value is 10766.19 and new Median value is 10766.19

### Are there differences in activity patterns between weekdays and weekends?

1. Create new factor weekday and weekend

```r
completedata$date <- as.Date(completedata$date)
#create a vector of weekdays
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
#convert to `factor` and specify the `levels/labels`
completedata$wDay <- factor((weekdays(completedata$date) %in% weekdays1), 
                   levels=c(FALSE, TRUE), labels=c('weekend', 'weekday')) 
```

2. Time serie panel plot interval (x-axis) and average steps taken by weekend and weekdays (y-axis)

```r
interval_weekend_weekday <- completedata %>% group_by(.dots=c("interval","wDay")) %>% 
                                                              summarise(mean_steps=mean(steps))
ggplot(data=interval_weekend_weekday,aes(x=interval,y=mean_steps)) + 
        geom_line() + facet_wrap(~wDay) + theme_bw() + xlab("Interval") +ylab("Mean Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
