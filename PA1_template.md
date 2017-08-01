---
title: "RepResearchAssgn1"
author: "Indradwip Dutta"
date: "August 1, 2017"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

### Reproducable Research Assignment1

### the document builds up the answers to Questions in Assignment1 step by step

#### Loading and preprocessing the data



####Show any code that is needed to Load the data (i.e. read.csv()) 

```{r }
##Read the data
StepsData<-read.table("activity.csv",header=TRUE,sep=",")
##check the data
head(StepsData)
```



###Q1.What is mean total number of steps taken per day?For this part of the assignment, you can ignore the missing values in the dataset.


#####      1.Calculate the total number of steps taken per day
 
```{r echo=TRUE,warning=FALSE}
library(dplyr)
##consider the cases without NA
StepsData_noNA<-StepsData[complete.cases(StepsData),]
head(StepsData_noNA)

##Table the sum total steps taken per day
StepsDay_Sum<-aggregate(steps~date,StepsData_noNA,sum)
head(StepsDay_Sum)
```
 
   
      
#####      2.If you do not understand the difference between a histogram and a barplot, research the difference           between them. Make a histogram of the total number of steps taken each day
      
```{r echo=TRUE,warning=FALSE}
##Make a histogram of the total number of steps taken each day
hist(StepsDay_Sum$steps,xlab="No. of Steps /Date(Day)",ylab="count",col="light green",border="red",main="Histogram for Total Steps/day")
``` 

      
      
#####      3.Calculate and report the mean and median of the total number of steps taken per day
      

```{r echo=TRUE,warning=FALSE}
##Report the mean and median of the Data
mean(StepsDay_Sum$steps)
median(StepsDay_Sum$steps)
```



##__________________________________________________________________________________
###Q2.What is the average daily activity pattern?


#####      1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of                         steps taken, averaged across all days (y-axis)
 
```{r echo=TRUE,warning=FALSE}
avg_5min<-aggregate(steps~interval,StepsData_noNA,mean)
##check the average 5 min steps data set
head(avg_5min)
plot(avg_5min$interval,avg_5min$steps,type="l",col="blue",xlab = "No. of Steps /5 mins",ylab = "count",main = "Frequency of steps/5 min")
```
 
   
#####     2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number          of steps?

      
```{r echo=TRUE,warning=FALSE}
##the interval which has maximum no of steps and the step count
MaxInterval<-avg_5min[which.max(avg_5min$steps),]
MaxInterval
``` 

##__________________________________________________________________________________
###Q3.Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

#####      1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows                     with NAs)
```{r echo=TRUE,warning=FALSE}
##the total no. of missing values in the activity dataset
sum(is.na(StepsData$steps))
``` 
      
      
      
#####      2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need         to be sophisticated. For example, you could use the mean/median for that day, or the mean for that           5-minute interval, etc.

####the strategy used here is that the NA values are replaced with the average value of the corresponding         Interval as calculated over all the years as in the table "avg_5min" above


#####      3.Create a new dataset that is equal to the original dataset but with the missing data filled in.


```{r echo=TRUE,warning=FALSE}
##define the imputed dataset as the main activity data at the outset
imputedStepsData <- StepsData 
for (i in avg_5min$interval) {
IntervalChosen<-avg_5min$steps[avg_5min$interval == i]
  
  imputedStepsData[imputedStepsData$interval == i & is.na(imputedStepsData$steps), ]$steps <- IntervalChosen
}
##check the imputed data set
head(imputedStepsData)

tail(imputedStepsData)
```




#####      4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and                          median total number of steps taken per day. Do these values differ from the estimates from the first         part             of the assignment? What is the impact of imputing missing data on the estimates of the total            daily                number of steps?

```{r echo=TRUE,warning=FALSE}
##Make a histogram of the total number of steps taken each day

imputedStepsData_Sum<-aggregate(steps~date,imputedStepsData,sum)
hist(imputedStepsData_Sum$steps,xlab="No. of Steps /Date(Day)",ylab="count",col="light green",border="red",main="Histogram for Total Steps/day")

mean(StepsDay_Sum$steps)#####mean with the original Activity data with NAs
median(StepsDay_Sum$steps)#####median with the original Activity data with NAs

mean(imputedStepsData_Sum$steps)#####mean with the Imputed Activity data without NAs
median(imputedStepsData_Sum$steps)#####median with the Imputed Activity data without NAs

#####difference in mean
paste(((mean(imputedStepsData_Sum$steps)-mean(StepsDay_Sum$steps))/mean(StepsDay_Sum$steps))*100,"%")

#####difference in median
paste(((median(imputedStepsData_Sum$steps)-median(StepsDay_Sum$steps))/median(StepsDay_Sum$steps))*100,"%")

``` 

##__________________________________________________________________________________
###Q4.Are there differences in activity patterns between weekdays and weekends?

    For this part the weekdays() function may be of some help here. Use the dataset with the filled-in  missing values for       this part.

#####       1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating                           whether a given date is a weekday or weekend day.
 
```{r echo=TRUE,warning=FALSE}
##new variable called imputedStepsData_wkday_wkend with daytype column
imputedStepsData_wkday_wkend<-imputedStepsData###set to last dataset at the outset

###convert date column to Date 
imputedStepsData_wkday_wkend$date<-as.Date(imputedStepsData_wkday_wkend$date)

###add a new column with daytype defined
imputedStepsData_wkday_wkend<-data.frame(imputedStepsData_wkday_wkend,daytype=weekdays(imputedStepsData_wkday_wkend$date))

###change levels of daytype to weekday or weekend
levels(imputedStepsData_wkday_wkend$daytype)<-c(rep("weekday",2),rep("weekend",2),rep("weekday",3))
head(imputedStepsData_wkday_wkend)
``` 
 
          
          
          
#####          2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval                                (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days                             (y-axis). See the README file in the GitHub repository to see an example of what this plot should                            look like using simulated data.
      
```{r echo=TRUE,warning=FALSE}
##call the lattice library
library(lattice)
##calulate the average no of steps per interval per day(weekday or weekend)
avg_step_wkday_wkend <- aggregate(steps ~ interval + daytype, data = imputedStepsData_wkday_wkend, mean)

##plot the lattice curve
xyplot(steps ~ interval | daytype, data = avg_step_wkday_wkend, type = "l", lwd = 2,layout = c(1, 2), xlab = "Intervals every 5 mins-Identifier", ylab = "Number of steps(averaged) by every interval every Day",
main = "Number of steps(averaged) by intervals & by weekend/weekdays")
``` 
 
      
 
      
