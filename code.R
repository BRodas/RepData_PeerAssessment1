
# Setting de envirnoment --------------------------------------------------

setwd("~/RepData_PeerAssessment1")
library("reshape2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.1")
library("ggplot2", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.1")

# Loading and preprocessing the data --------------------------------------

# Before loading the data I had to unzip the file with it,
# The command I used was: unzip activity.zip
activity <- read.csv("~/RepData_PeerAssessment1/activity.csv", row.names=NULL)
# now I'm gonna change the format
activity$date <- as.Date(activity$date, format = "%Y-%m-%d")



# What is mean total number of steps taken per day? -----------------------

# Total number of Steps taken by day
nsteps_total <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
# Histogram of the total number of steps taken each day
hist(nsteps_total, breaks = 10)
# Calculate mean and median of the total number of steps taken per day
mean(nsteps_total)
abline(v=mean(nsteps_total), col="red")
median(nsteps_total)
abline(v=median(nsteps_total), col="green")


# What is the average daily activity pattern? -----------------------------

# Make a time series plot (i.e. type = "l") 
# of the 5-minute interval (x-axis) 
# and the average number of steps taken, averaged across all days (y-axis)
nsteps_every5min <- tapply(activity$steps
                           , activity$interval, mean, na.rm=TRUE)
plot(x=dimnames(nsteps_every5min)[[1]] 
     , y=nsteps_every5min, type="l", col="red")

# Which 5-minute interval, on average across all the days in the dataset, 
# contains the maximum number of steps?
which.max(nsteps_every5min)


# Imputing missing values -------------------------------------------------

# Calculate and report the total number of missing values in the dataset 
# (i.e. the total number of rows with NAs)
table(complete.cases(activity))

# Devise a strategy for filling in all of the missing values in the dataset. 
# The strategy does not need to be sophisticated. For example, 
# you could use the mean/median for that day, 
# or the mean for that 5-minute interval, etc.
# Create a new dataset that is equal to the original dataset 
# but with the missing data filled in.
interval_to_impute <- unique(activity[!complete.cases(activity),"interval"])

activity_2 <- activity

for(i in interval_to_impute){
    imp_value <- nsteps_every5min[unlist(dimnames(nsteps_every5min))==i]
    activity_2[!complete.cases(activity_2) 
             & activity_2$interval==i,"steps"] <- imp_value
}

# Make a histogram of the total number of steps taken each day 
# and Calculate and report the mean and median total number of steps 
# taken per day. Do these values differ from the estimates 
# from the first part of the assignment? What is the impact 
# of imputing missing data on the estimates of the total daily number 
# of steps?
nsteps_total_2 <- tapply(activity_2$steps, activity_2$date, sum, na.rm=TRUE)
hist(nsteps_total_2, breaks = 10)
mean(nsteps_total_2)
abline(v=mean(nsteps_total_2), col="red")
median(nsteps_total_2)
abline(v=median(nsteps_total_2), col="green")


# Differences in activity patterns between weekdays and weekends ----------

# Create a new factor variable in the dataset with two levels – “weekday” 
# and “weekend” indicating whether a given date is a weekday or weekend day.
activity_2$day <- weekdays(activity_2$date)

weekday <- activity_2$day %in% c("Monday", "Tuesday"
                                , "Wednesday", "Thursday", "Friday")
weekend <- activity_2$day %in% c("Saturday", "Sunday")

activity_2[weekday, "weekday"] <- "Weekday" 
activity_2[weekend, "weekday"] <- "Weekend" 

activity_2$weekday <- as.factor(activity_2$weekday)

# Make a panel plot containing a time series plot (i.e. type = "l")
# of the 5-minute interval (x-axis) and the average number of steps taken, 
# averaged across all weekday days or weekend days (y-axis).
# See the README file in the GitHub repository to see an example 
# of what this plot should look like using simulated data.
steps_2levels<- with(activity_2
                     , tapply(steps, list(interval, weekday), mean))
steps_2levels <- as.data.frame(steps_2levels)
steps_2levels$interval <- as.integer(rownames(steps_2levels))

reshaped_steps_2levels <- reshape(as.data.frame(steps_2levels), 
                                  varying = c("Weekday", "Weekend"),
                                  v.names = "Mean_Steps",
                                  timevar = "day_type",
                                  times = c("Weekday", "Weekend"),
                                  direction = "long")

qplot(x=interval, y=Mean_Steps, data = reshaped_steps_2levels
      , geom=c("path"), facets= day_type~., ylab = "Mean Steps")

