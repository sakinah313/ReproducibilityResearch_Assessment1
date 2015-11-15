#reading the data 
setwd("~/Documents/MOOCDataScience/Reproduce")
mydata <- read.csv("activity.csv")
names(mydata)
head(mydata, n=10)
summary(mydata)
# dealing with missing data
# complete.cases - Return a logical vector indicating which cases are complete, have no missing values.
CompleteData<-complete.cases(mydata)   
# number of missing data 
nMissingData <- length(CompleteData[CompleteData==FALSE])  
# number of Complete data 
nCompleteData <- length(CompleteData[CompleteData==TRUE])  
title="Boxplot : Missing vs. Complete Cases"  
barplot(table(CompleteData),main=title,xaxt='n' ) 
axis(side=1,at=c(.7,1.9),labels=c("Missing Data","Complete Data"),tick=TRUE)           
# label the barplot  
text(.7,0,labels=nMissingData, pos=3)                                            
text(1.9,0,labels=nCompleteData, pos=3)                                     


#What is mean total number of steps taken per day?
CleanData<-subset(mydata,complete.cases(mydata))
GroupByDay<-split(CleanData,CleanData$date,drop=TRUE)
# build a numeric vector w/ daily sum of steps
DailySteps<-sapply(GroupByDay,function(x) sum(x$steps))
# plot a histogram 
hist(DailySteps, main="Hist Total Steps per Day", xlab="# Steps", col="bisque3")  

abline(v=mean(DailySteps), lty=3, col="blue")                   # draw a blue line thru the mean  
abline(v=median(DailySteps), lty=4, col="red")                  # draw a red line thru the median  
text(mean(dailySteps),25,labels="mean", pos=4, col="blue")      # label the mean  
text(mean(dailySteps),23,labels="median", pos=4, col="red")     # label the median  
rug(dailySteps, col="chocolate")                       
hist(mydata$steps) #computes a histogram of the data values in the column steps
summary(DailySteps)

#What is the average daily activity pattern?
SplitByInterval<-split(CleanData,CleanData$interval,drop=TRUE)
intervalAverage<-sapply(SplitByInterval,function(x) mean(x$steps))
plot(intervalAverage, type = "l",
     main = "Interval", 
     ylab = "Average",
     xlab = "Interval")
abline(v=which.max(intervalAverage), lty=3, col="blue") 
text(which.max(intervalAverage),max(intervalAverage),  
     labels=paste("max = ",as.character(round(max(intervalAverage)))), 
     pos=4, col="blue")


names(which.max(intervalAverage))
round(max(intervalAverage))
which.max(intervalAverage)


#Imputing missing values
#$df = data.frame(x = 1:20, y = c(1:10,rep(NA,10)))
mydata$steps[is.na(mydata$steps)] = mean(mydata$steps, na.rm=TRUE)

ImpuData = mydata
#mydata <- read.csv("activity.csv")

splitNewByDay <- split(ImpuData,ImpuData$date, drop=TRUE)                  # split the newData by date  
dailyStepsNew <- sapply(splitNewByDay, function(x) sum(x$steps))         # numeric vector w/ daily sum of steps  
hist(dailyStepsNew, main="NEW Hist: Total Steps per Day", xlab="         # Steps", col="bisque3") # plot a histogram  
abline(v=mean(dailySteps), lty=3, col="blue")                            # draw a blue line thru the mean  
abline(v=median(dailySteps), lty=4, col="red")                           # draw a red line thru the median  
text(mean(dailySteps),35,labels="mean", pos=4, col="blue")               # label the mean  
text(mean(dailySteps),33,labels="median", pos=4, col="red")              # label the median  

summary(dailyStepsNew)

par(mfrow=c(1,2))

### plot the original histogram
hist(DailySteps, main="Hist Total Steps per Day", xlab="# Steps", col="bisque3", ylim=c(0,35)) # plot a histogram  
abline(v=mean(DailySteps), lty=3, col="blue")                      # draw a blue line thru the mean  
abline(v=median(DailySteps), lty=4, col="red")                     # draw a red line thru the median  
text(mean(DailySteps),25,labels="mean", pos=4, col="blue")         # label the mean  
text(mean(DailySteps),23,labels="median", pos=4, col="red")        # label the median  


### plot the imputed histogram
hist(dailyStepsNew, main="NEW Hist: Total Steps per Day", xlab="# Steps", col="bisque3", ylab="") # plot a histogram  
abline(v=mean(dailySteps), lty=3, col="blue")                      # draw a blue line thru the mean  
abline(v=median(dailySteps), lty=4, col="red")                     # draw a red line thru the median  
text(mean(dailySteps),35,labels="mean", pos=4, col="blue")         # label the mean  
text(mean(dailySteps),33,labels="median", pos=4, col="red")        # label the median  
rug(dailyStepsNew,col="chocolate")

mydata$date <- as.Date(strptime(mydata$date, format="%Y-%m-%d")) # convert date to a date() class variable  
mydata$day <- weekdays(mydata$date)                              # build a 'day' factor to hold weekday / weekend  
for (i in 1:nrow(mydata)) {                                       # for each day  
  if (mydata[i,]$day %in% c("Saturday","Sunday")) {             # if Saturday or Sunday,
    mydata[i,]$day<-"weekend"                                 #   then 'weekend'
  }
  else{
    mydata[i,]$day<-"weekday"                                 #    else 'weekday'
  }
}

## aggregate newData by steps as a function of interval + day  
stepsByDay <- aggregate(mydata$steps ~ mydata$interval + mydata$day, mydata, mean)

## reset the column names to be pretty & clean
names(stepsByDay) <- c("interval", "day", "steps")

## plot weekday over weekend time series
par(mfrow=c(1,1))  
with(stepsByDay, plot(steps ~ interval, type="n", main="Weekday vs. Weekend Avg."))  
with(stepsByDay[stepsByDay$day=="weekday",], lines(steps ~ interval, type="l", col="chocolate"))  
with(stepsByDay[stepsByDay$day == "weekend",], lines(steps ~ interval, type="l", col="16" ))  
legend("topright", lty=c(1,1), col = c("chocolate", "16"), legend = c("weekday", "weekend"), seg.len=3)

