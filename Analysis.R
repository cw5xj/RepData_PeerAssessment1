library(dplyr)
#Loading the data to the data frame activity
if(!file.exists("activity.csv")) unzip("activity.zip")
activity<-read.csv("activity.csv",colClasses = c("integer","character","integer"))
str(activity)

##############################################################################
#Process/transform the data into a format suitable for the analysis

#1:Transform the date from character into date
#2:Add the WeekDay column to tell whether the day is weekday or weekend
activity$date<-as.Date(activity$date)
activity<-mutate(activity,WeekDay=as.factor(sapply(activity$date,function(x){wd<-weekdays(x);
  ifelse(wd=="Saturday"|wd=="Sunday","Weekend","Weekday")})))

##############################################################################
#What is mean total number of steps taken per day?

DailySum<-aggregate(steps~date,activity,sum,na.rm=TRUE)#Aggregate the total number of steps taken each day
par(mfrow=c(1,1),mar=c(4,4,5,1))
hist(DailySum$steps,breaks=10,xlab="Total # of steps each day",main="distribution of daily steps
     before filing missing values") 
dev.copy(png,"Hist1.png")
dev.off()
mean(DailySum$steps)   #mean of # of steps per day:10766
median(DailySum$steps) #median of # of steps per day:10765

##############################################################################
#What is the average daily activity pattern?

steps_by_interval<-aggregate(steps~interval,activity,mean,na.rm=TRUE)#Aggregate the average number of steps taken each interval
plot(steps_by_interval$interval,steps_by_interval$steps,type="l",xlab="interval",
     ylab="average # of steps",main="Daily activity pattern")
dev.copy(png,"DailyPattern.png")
dev.off()
steps_by_interval[steps_by_interval$steps==max(steps_by_interval$steps),1] #the interval with maximum steps

##############################################################################
#Imputing missing values: filling missing values with mean of that 5-minute interval

sum(is.na(activity$steps)) #total number of missing values
ndx<-which(is.na(activity$steps))#index of observations with missing values
activity_fillNA<-activity #create a new dataset for filling NA values
for(i in ndx) {#filling missing values with mean of that 5-minute interval
  activity_fillNA$steps[i]<-steps_by_interval[steps_by_interval$interval==activity_fillNA$interval[i],2]
}
DailySum_fillNA<-aggregate(steps~date,activity_fillNA,sum)#Aggregate the total number of steps taken each day
hist(DailySum_fillNA$steps,breaks=10,xlab="Total # of steps each day",main="distribution of daily steps after filling 
  missing values") 
dev.copy(png,"Hist2.png")
dev.off()
mean(DailySum_fillNA$steps) #mean of # of steps per day:10766
median(DailySum_fillNA$steps)#median of # of steps per day:10766

##############################################################################
#Are there differences in activity patterns between weekdays and weekends?
steps_interval_WD<-aggregate(steps~WeekDay+interval,data=activity_fillNA,mean)
steps_interval_WD$WeekDay<-as.character(steps_interval_WD$WeekDay)
WeekDay<-subset(steps_interval_WD,WeekDay=="Weekday")
WeekEnd<-subset(steps_interval_WD,WeekDay=="Weekend")
par(mfrow=c(2,1),mar=c(4,4,2,1))
plot(WeekDay$interval,WeekDay$steps,type="l",xlab="interval",
     ylab="average # of steps",main="WeekDay daily activity pattern")
plot(WeekEnd$interval,WeekEnd$steps,type="l",xlab="interval",
     ylab="average # of steps",main="WeekEnd daily activity pattern")
dev.copy(png,"Compare.png")
dev.off()

