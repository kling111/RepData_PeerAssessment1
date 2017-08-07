# Peer Assignment 1
Karan Lingineni  
8/5/2017  



## Purpose

This is my R Markdown for the first project assignment for Coursera's Reproducible Research. The point of this project is to practice loading, cleaning, and analyzing a data set.

## Loading and Processing the Data

Here, I make sure the zip file containing activity.csv is present on my computer. Then I unzip the file, and load the csv into a seperate variable:


```r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
        temp <- tempfile()
        download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
        unzip(temp)
        unlink(temp)
}

data <- read.csv("activity.csv")
```

## Mean Number of Steps per Day

Here, I create a histogram that graphs the total number of steps per day. From this graph, I can calculate the mean and median number of steps per day.


```r
stepsPerDay <- aggregate(steps ~ date, data, sum)
hist(stepsPerDay$steps, 
     main = "Total Steps Taken per Day", 
     col = "red", 
     xlab = "Steps")
```

![](PA1_files/figure-html/graph1-1.png)<!-- -->

```r
stepsMean <- mean(stepsPerDay$steps)
stepsMedian <- median(stepsPerDay$steps)
```

Mean Number of Steps per Day: 

```
## [1] 10766.19
```
Median Number of Steps per Day: 

```
## [1] 10765
```

## Average Daily Activity Pattern

Here, I create a plot that shows the average number of steps, over five days, in a given interval.


```r
stepsByInterval <- aggregate(steps ~ interval, data, mean)
plot(stepsByInterval$interval, stepsByInterval$steps, type = "l", xlab = "Interval", ylab = "Steps", main = "Avergage Number of Steps per Day by Interval")
```

![](PA1_files/figure-html/interval-1.png)<!-- -->

```r
maxInterval <- stepsByInterval[which.max(stepsByInterval$steps), 1]
```

Average interval with the highest steps: 

```
## [1] 835
```

## Imputing Missing Data

My approach for filling in the missing data in the file, or "NA"s, was to use the average steps for the interval that missing value was contained in. For example, if there is a missing value in interval 200, then I compute the average steps for that interval and replace "NA" with that number. 


```r
missing <- sum(!complete.cases(data))
input <- transform(data, 
                   steps = ifelse(is.na(data$steps), 
                   stepsByInterval$steps[match(data$interval, stepsByInterval$interval)], 
                   data$steps))
input[as.character(input$date) == "2012-10-01", 1] <- 0
```

Create a comparative histogram:


```r
newStepsPerDay <- aggregate(steps ~ date, input, sum)
hist(newStepsPerDay$steps, 
     main = paste("Total Steps Each Day"), 
     col="green", 
     xlab="Number of Steps")
hist(stepsPerDay$steps, 
     main = paste("Total Steps Each Day"), 
     col="red", 
     xlab="Number of Steps", 
     add=T)
legend("topright", c("Adjusted", "Non-adjusted"), col=c("green", "red"), lwd=10)
```

![](PA1_files/figure-html/compare-1.png)<!-- -->

Calculate new mean and median:


```r
newMean <- mean(newStepsPerDay$steps)
newMedian <- median(newStepsPerDay$steps)
```

Calculate the difference between new and old values:


```r
diffMean <- newMean - stepsMean
medianDiff <- newMedian - stepsMedian
totalDiff <- sum(newStepsPerDay$steps) - sum(stepsPerDay$steps)
```

Summary of calculated values:


```r
##New Mean
print(newMean)
```

```
## [1] 10589.69
```

```r
##New Median
print(newMedian)
```

```
## [1] 10766.19
```

```r
##Difference in means
print(diffMean)
```

```
## [1] -176.4949
```

```r
##Difference in medians
print(medianDiff)
```

```
## [1] 1.188679
```

```r
##Difference in total steps
print(totalDiff)
```

```
## [1] 75363.32
```

## Difference in activity patterns between weekends and weekdays


```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
input$dow = as.factor(ifelse(is.element(weekdays(as.Date(input$date)),weekdays), "Weekday", "Weekend"))

newStepsPerInterval <- aggregate(steps ~ interval + dow, input, mean)

library(lattice)

xyplot(newStepsPerInterval$steps ~ newStepsPerInterval$interval|newStepsPerInterval$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_files/figure-html/difference-1.png)<!-- -->

As you can see from these two graphs, there is a higher peak on the "Weekdays" graph, while there is overall more activity on the "Weekends" graph. Activity also seems to begin earlier during the weekdays, as you can tell from its graph.
