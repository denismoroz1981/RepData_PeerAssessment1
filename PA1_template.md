PA1\_template
================

Report
======

Data loading
------------

``` r
dataURL<-"https://raw.githubusercontent.com/denismoroz1981/RepData_PeerAssessment1/master/activity.csv"
mData<-read.csv(dataURL)
```

What is mean total number of steps taken per day?
-------------------------------------------------

Histogram with total numbers per days is below.

``` r
library(plyr)
mDataSumm<-ddply(mData,"date",summarise,total=sum(steps,na.rm = TRUE))
hist(mDataSumm$total)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

``` r
mDataSummMean<-mean(mDataSumm$total)
mDataSummMedian<-median(mDataSumm$total)
```

Mean of total number of steps taken per day is 9354.2295082, the mediam is 10395.

What is the average daily activity pattern?
-------------------------------------------

Plot of 5-minute interval and the average number steps taken is below.

``` r
mDataAverage<-ddply(mData,"interval",summarise,mean=mean(steps,na.rm = TRUE))
plot(mDataAverage$interval,mDataAverage$mean,type = "l")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-4-1.png)

Interval containing the maximum number of steps on average is below.

``` r
mDataAverage[which.max(mDataAverage$mean),]
```

    ##     interval     mean
    ## 104      835 206.1698

Imputing missing values
-----------------------

``` r
countNA<-sum(is.na(mData$steps))
```

Total number of missing values is 2304

Strategy for filling in missing values is the mean for respective 5 minute interval.

``` r
mDataImp<-mData
filling<-function(steps,interval){
 mean<-mean(mDataImp$steps[mDataImp$interval==interval],na.rm=TRUE)
 stepsF<-ifelse(is.na(steps),mean,steps)
 return(stepsF)
}
mDataImp$steps<-mapply(filling,mDataImp$steps,mDataImp$interval)
```

Histogram of total step taking each date is below.

``` r
mDataImpSumm<-ddply(mDataImp,"date",summarise,total=sum(steps,na.rm = TRUE))
hist(mDataImpSumm$total)
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
mDataImpSummMean<-as.integer(mean(mDataImpSumm$total))
mDataImpSummMedian<-as.integer(median(mDataImpSumm$total))
```

Having missing vlues imputed, mean of total number of steps taken per day is 10766, the mediam is 10766. The values are higher then when calculated w/o imputing missing values.

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

``` r
mDataImpWW<-mDataImp
mDataImpWW$date<-as.Date(mDataImpWW$date)
mDataImpWW$WW<-lapply(mDataImpWW$date,weekdays)
isweekend<-function(x){
  if (x %in% c("субота","неділя","Sunday","Saturday")) result<-"weekend" else result<-"weekday"
}
mDataImpWW$isweekend<-lapply(mDataImpWW$WW,isweekend)
mDataImpWW$isweekend<-unlist(mDataImpWW$isweekend)
mDataImpWWSumm<-ddply(mDataImpWW,c("interval","isweekend"),summarise,average=mean(steps))
library(lattice)
xyplot(average~interval|isweekend,data=mDataImpWWSumm,layout=c(1,2),type="l")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-9-1.png)
