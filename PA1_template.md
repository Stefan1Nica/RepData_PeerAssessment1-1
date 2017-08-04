# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
Loading data into environment :

```r
setwd("C:/workspace/R training/RepData_PeerAssessment1")
dataset <- read.csv("activity.csv", head = TRUE, na.strings = "NA")
summary(dataset)
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
Proocessing the data: changing data formats into usable formats and cutting out missing values

```r
dataset$date <- as.Date(dataset$date)
datasetclean <- subset(dataset, !is.na(dataset$steps))
```

## What is mean total number of steps taken per day?
### I will show this in two ways:
1. Calculating the mean and the median of the total number of steps taken per day

```r
daily <- tapply(datasetclean$steps, datasetclean$date, sum, na.rm = TRUE, simplify = TRUE)
day <- daily[!is.na(daily)]
mean(day)
```

```
## [1] 10766.19
```

```r
median(day)
```

```
## [1] 10765
```

- plotting a histogram to see a distribution of this characteristic

```r
dailysum <- tapply(datasetclean$steps, datasetclean$date, sum, na.rm=TRUE, simplify=T)
dailysum <- dailysum[!is.na(dailysum)]

hist(x=dailysum,
     col="red",
     breaks=20,
     xlab="Daily total steps",
     ylab="Frequency",
     main="The distribution of daily total (missing data ignored)")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
