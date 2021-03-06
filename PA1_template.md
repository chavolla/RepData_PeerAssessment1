# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data



The data coming from the measurement of the steps froma a subject from 2 months are loaded into memory.  
Also the class of each variable is given to help in the data process:


```r
#If the file is not present, it would unzipped
if (!file.exists("activity.csv")) {
    unzip("activity.zip")
}
data<-read.csv("activity.csv", colClasses=c("integer","POSIXct","character"))
```

 The loaded dataset has **3 variables** with **17568 observations**. A quick look into the data:


```r
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
  
  
  
## What is mean total number of steps taken per day?

The missing values are removed in order to process the total number of steps found in the measurements. From the clean dataset, a histogram is created.


```r
#Removing NA values
dataStepsClean<-data[!is.na(data$steps),]
#Calculating the total number of steps perday
dayStepTotal<-tapply(dataStepsClean$steps, dataStepsClean$date,sum)
#Calculating the mean and Median
stepsMean <-mean(dayStepTotal)
stepsMedian<- median(dayStepTotal)
#Plotting the histogram
hist(dayStepTotal, col="red", xlab="total of steps per day", main="Total steps per day histogram")
#adding mean and median to the plot
abline(v=stepsMean,col="aquamarine",lty=1, lwd=5)
abline(v=stepsMedian,col="blue",lty=1, lwd=2)
legend("topleft",legend = c(sprintf("Mean: %.04f",stepsMean), sprintf("Median: %.04f",stepsMedian)),col=c("aquamarine","blue"), lty=1, cex=0.8, bty="n")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

The resulting **Mean** is: **10766.1887** and the **Median** is: **10765.0000**

## What is the average daily activity pattern?


The avarage of the steps in each time interval across the days is calculated:


```r
intervalStepMeans<- setNames(aggregate(steps~as.numeric(interval),dataStepsClean, mean),c("interval","steps"))
```


A timeline graphic is created to see the Step Mean during the time intervals, so the maximum activity point can be detected:



```r
#lattice library is used for time series
library(lattice)
#Formating Interval Function
formatInterval<-function(x){ gsub('^(\\d{2})(\\d+)$', '\\1:\\2',sprintf("%04d", x))}
#Formating x scale
xscale.components.custom = function(...) {
     ans <- xscale.components.default(...)
     ans$bottom$labels$labels <- formatInterval(ans$bottom$labels$at)
     ans
}
#Obtaining the maximums values
maxStepMean<-max(intervalStepMeans$steps)
intervatWithMaxMean<-intervalStepMeans[intervalStepMeans["steps"]==maxStepMean,][,"interval"]
#Creating plot
xyplot(steps~interval, data=intervalStepMeans, layout = c(1,1),scales = list(x=list(rot = 45,abbreviate=FALSE,tick.number=10),y=list(tick.number=10)), xscale.components = xscale.components.custom, xlab="Time interval",ylab="Steps Average", panel = function(x, y) {
         panel.xyplot(x, y,  type=c("l","g"),)         
         panel.points(x[y==max(y)], max(y),pch=19, col="red")
         panel.abline( h=max(y), lty = "dotted", col = "black")
         panel.abline( v=x[y==max(y)], lty = "dotted", col = "black")
         panel.text(x[y==max(y)]+90,1,labels=formatInterval(intervalStepMeans$interval[y==max(y)]))
         panel.text(1,max(y)-10,labels=sprintf("%.2f", intervalStepMeans$steps[y==max(y)]) )
         panel.lines(x[y==max(y)], max(y))
       } )
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 


The maximum **Step Mean** is:  **206.1698**, found at **Time Interval** : **08:35**


## Imputing missing values



Calculating missing values


```r
missingValues<- sum( is.na(data$steps))
```


In the dataset were found **2304** values.  

In order to replace the missing values, it is used the **Mean value for each time interval across the days**. It is expected that the mean is not affected, but some other minors effect could appear. So in order to apply this strategy the following code is implemented:



```r
data2<-as.data.frame(t(apply(data,1,FUN=function(x){
  if(is.na(x["steps"])){
      x["steps"]<-as.numeric(intervalStepMeans[intervalStepMeans["interval"]==x["interval"],][,"steps"])
    }
    x
  })), stringsAsFactors=FALSE)

#Since all the columns are returned as strings, the steps are converted to numeric values
#Also the date is transformed into a POSIXlt value
data2<-transform(data2, steps = as.numeric(steps),date=strptime(date,"%Y-%m-%d"), interval = as.numeric(interval))
```

in order to verify how the completion of the missing values affects the data behavior, 
the total steps per day metrics are repeated  

The histogram of the new dataset with NA values replaced (**after the strategy was applied**) against the previous Histogram where the NA were removed (**without the strategy**):


```r
#Calculating the new total steps per day
dayStepTotal2<-tapply(data2$steps, as.character(data2$date),sum)

#Calculating the mean and median of the new dataset
stepsMean2 <-mean(dayStepTotal2)
stepsMedian2 <- median(dayStepTotal2)

par(mfrow=c(1,2))
#Plotting the previous histogram
hist(dayStepTotal, col="red", xlab="total of steps per day (Before)", main="")
title(main = list("Total steps per day histogram (NA Removed)", cex = 0.8))
abline(v=stepsMean,col="aquamarine",lty=1, lwd=5)
abline(v=stepsMedian,col="blue",lty=1, lwd=2)
legend("topright",legend = c(sprintf("Mean: %.04f",stepsMean), sprintf("Median: %.04f",stepsMedian)),col=c("aquamarine","blue"), lty=1, cex=0.5, bty="n")
#plotting the new Histogram
hist(dayStepTotal2, col="red", xlab="total of steps per day (After)", main="")
title(main = list("Total steps per day histogram (NA Replaced)", cex = 0.8))
abline(v=stepsMean2,col="aquamarine",lty=1, lwd=5)
abline(v=stepsMedian2,col="blue",lty=1, lwd=2)
legend("topright",legend = c(sprintf("Mean: %.04f",stepsMean2), sprintf("Median: %.04f",stepsMedian2)),col=c("aquamarine","blue"), lty=1, cex=0.5, bty="n")
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 


Results in **Mean** is: **10766.1887** and the **Median** is: **10766.1887**  

Performing a comparison against the previous values obtained:

| Dataset                | Mean             |Median             |
| -----------------------|----------------- | ----------------- |
| **NA Removed (Before)**  | *10766.1887*  | *10765.0000* |
| **NA Replaced (After)**  | *10766.1887* | *10766.1887*|


There are no variation regarding the mean value, but the Histogram and the Median were slightly modified.



## Are there differences in activity patterns between weekdays and weekends?


For each observation, it is identified if the day is at weekend (Satuday or Sunday), o at weekday (Monday-Friday)  


```r
#Returning the plot configuration
par(mfrow=c(1,1))
#A new Column is added. this column is factor with two levels: "weekend" and "weekday"
data2<-transform(data2, dayType=as.factor(sapply(date$wday, FUN =function(x){
    if(x %in% c(0,6)){"weekend"}else{"weekday"} 
  } )))
```


The mean values for each day type at every interval is calculated


```r
data3<-aggregate(steps~dayType+interval, data2,mean)
```


The comparison between weekdays and weekend regarding the mean of the steps for each interval, it shows the different maximums for each day type.


```r
#plotting

xyplot(steps~interval|dayType, data=data3,  layout = c(1,2), scales = list(x=list(rot = 45,abbreviate=FALSE,tick.number=10),y=list(tick.number=10)), xscale.components = xscale.components.custom, xlab="Time interval",ylab="Steps Average", panel = function(x, y) {
         panel.xyplot(x, y,  type=c("l","g"),)         
         panel.points(x[y==max(y)], max(y),pch=19, col="red")
         panel.abline( h=max(y), lty = "dotted", col = "black")
         panel.abline( v=x[y==max(y)], lty = "dotted", col = "black")
         panel.text(x[y==max(y)]+90,1,labels=formatInterval(data3$interval[data3$steps==max(y)]))
         panel.text(1,max(y)-10,labels=sprintf("%.2f", data3$steps[data3$steps==max(y)]) )
         panel.lines(x[y==max(y)], max(y))
       } )
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png) 


