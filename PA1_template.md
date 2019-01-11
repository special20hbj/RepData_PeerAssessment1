---
title: 'Reproducible Research: Peer Assessment 1'
author: "Binjie HU"
date: "2019/1/4"
output: html_document
---

## Loading and preprocessing the data

```{r, echo = TRUE}
activity <- read.csv("/Users/HBJ/Documents/Coursera/Reproducible Research/Project 1/activity.csv", header = TRUE)
```

## What is mean total number of steps taken per day?

```{r, echo = TRUE}
library(plyr)
library(ggplot2)
totalSteps <- ddply(activity, "date", summarise, total = sum(steps, na.rm = TRUE))
g <- ggplot(totalSteps, aes(total))
p <- g + geom_histogram(binwidth = 1000) + xlab("Total number of steps") + ylab("Frequency") + labs(title = "Histogram of total steps taken per day") 
print(p)
mean_totalSteps <- mean(totalSteps$total)
median_totalSteps <- median(totalSteps$total)
```

* The **mean** total number of steps taken per day is `r mean(totalSteps$total)` steps.
* The **median** total number of steps taken per day is `r median(totalSteps$total)` steps.

## What is the average daily activity pattern?

```{r, echo = TRUE}
StepsInterval <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)
max_steps <- StepsInterval[which.max(StepsInterval$steps), ]
with(StepsInterval,{
        plot(interval, steps, type = "l", main = "Average Daily Activity Pattern", xlab = "5-minute interval", ylab = "Average number of steps taken")
        points(max_steps$interval, max_steps$steps, col = "red", lwd = 2, pch = 19)
        legend("topright", legend = "the maximum number of steps", text.col = "red", bty = "n")
})
```

* The 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps is  `r StepsInterval[which.max(StepsInterval$steps), ]$interval`.

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset.
```{r echi = TRUE}
totalNA <- sum(is.na(activity$steps))
```

* The **total number** of missing values in the dataset is `r sum(is.na(activity$steps))`.

2.Devise a strategy for filling in all of the missing values in the dataset. 
3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r echo = TRUE}
## All of the missing values would be filled in with the mean for that 5-minute interval ##
avgInterval_steps <- function(interval){
        StepsInterval[StepsInterval$interval == interval, ]$steps
}
filled_value <- function(steps, interval) {
        filling_steps <- NA
        if(!is.na(steps))
                filling_steps <- steps
        else
                filling_steps <- avgInterval_steps(interval)
        return(filling_steps)
}
filled_activity <- activity
filled_activity$steps <- mapply(filled_value, filled_activity$steps, filled_activity$interval)
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```{r echo = TRUE}
library(plyr)
library(ggplot2)
filled_totalSteps <- ddply(filled_activity, "date", summarise, filled_total = sum(steps))
g <- ggplot(filled_totalSteps, aes(filled_total))
p <- g + geom_histogram(binwidth = 1000) + xlab("Total number of steps") + ylab("Frequency") + labs(title = "Histogram of total steps taken per day (Missing values imputed)") 
print(p)
mean_totalSteps2 <- mean(filled_totalSteps$filled_total)
median_totalSteps2 <- median(filled_totalSteps$filled_total)
```

* The **mean** total number of steps taken per day (with missing values imputed) is `r mean(filled_totalSteps$filled_total)` steps.
* The **median** total number of steps taken per day (with missing values imputed) is `r median(filled_totalSteps$filled_total)` steps.
* The mean and median values are almost the same, and are both **higher** than that in the first part of the assignment after imputing missing data with the mean for that 5-minute interval. 
* When imputing missing data on the estimates of the total daily number of steps with the mean steps for particular 5-minute interval, the NA values which are set to be 0s by default in calculating are replaced by specific values so that the total numbers of steps taken per day are increased.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```{r echo = TRUE}
## Change the console language to English ##
system("defaults write org.R-project.R force.LANG en_US.UTF-8")
filled_activity$day <- weekdays(as.Date(filled_activity$date))
week_character <- function(day){
        if(day %in% c("Saturday", "Sunday"))
                return("weekend")
        else
                return("weeday")
}
filled_activity$weektype <- sapply(filled_activity$day, week_character)
```

2.Make a panel plot containing a time series plot (i.e. 
type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```{r echo = TRUE}
library(lattice)
filled_StepsInterval <- aggregate(steps ~ interval + weektype, filled_activity, mean, na.rm = TRUE)
xyplot(steps ~ interval | weektype, filled_StepsInterval, type = "l", xlab = "the 5-minute interval", ylab = "the average number of steps", main = "Average Daily Activity Pattern Between Weekdays and Weekends", layout = c(1,2))
```