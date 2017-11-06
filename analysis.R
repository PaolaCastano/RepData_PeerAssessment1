## Reproducible Research: Peer Assessment 1

## This assignment makes use of data from a personal activity monitoring
## device. This device collects data at 5 minute intervals through out the
## day. The data consists of two months of data from an anonymous
## individual collected during the months of October and November, 2012
## and include the number of steps taken in 5 minute intervals each day.

## R script that performs the next steps:
## 1. Code for reading in the dataset and/or processing the data
## 2. Histogram of the total number of steps taken each day
## 3. Mean and median of the total number of steps taken each day
## 4. Time series plot of the average number of steps taken 
## 5. The 5-minute interval that, on average, contains the maximum number of steps
## 6. Code to describe and show a strategy for imputing missing data
## 7. Histogram of the total number of steps taken each day after missing values are imputed
## 8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends
## 9. All of the R code needed to reproduce the results (numbers, plots, etc.) in the report
## 
## By Paola Castaño
## 
## The script assumes that the data zip file has been downloaded and unzipped already.
## If not, please run the next code to get the raw data. 
## (Remember to set destfile according to your working directory)
        
        # urlfile<-"https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        # download.file(urlfile, destfile, method = curl)
        # unzip(destfile, list = TRUE)

# Load packages into the sesion
library(dplyr) 
library(ggplot2)
library(lubridate)
library(stringr)

# Create directory to save plots
if (!dir.exists("./figure")){        
        dir.create("./figure")
}

# Question: Loading and preprocessing the data
        # 1. Code for reading in the dataset and/or processing the data
        # Load dataset into R object
        activity<-read.csv("./activity.csv", colClasses=c("integer","Date","integer"))
        
        # Process/transform the data (if necessary) into a format suitable for your analysis
        # Format interval column to time(h:m:s)
        time.padded <- str_pad(activity$interval, 4, pad="0")
        activity$timeint<-hm(sub("(..)$", ":\\1", time.padded))

# Question: What is mean total number of steps taken per day?
        # Ignore the missing values in the dataset for now
        steps.day <- tapply(activity$steps, activity$date, sum, na.rm=TRUE)
        
        # 2. Histogram of the total number of steps taken each day
        # Make a histogram of the total number of steps taken each day
        # Open png device, create 'hist1.png' in the working directory
        png(filename = "./figure/hist1.png",  width=640, height=480)
        
        g1<-qplot(steps.day, 
              xlab='Total steps per day', 
              ylab='Frequency', 
              ylim = c(0,13),
              main = 'Total number of steps taken each day',
              fill=I("dodgerblue"), 
              col=I("blue"),
              binwidth=600)
        
        print(g1)
        
        # Close the png file device
        dev.off()
        
        # 3. Mean and median of the total number of steps taken each day
        # Calculate and report the mean and median total number of steps taken per day
        mean.steps.day <- mean(steps.day)
        median.steps.day <- median(steps.day)

# Question: What is the average daily activity pattern?
        average.day <- 
                activity %>%
                group_by(interval) %>%
                summarize(steps=mean(steps, na.rm = TRUE))
        # Format interval column to time(h:m:s)
        time.padded2 <- str_pad(average.day$interval, 4, pad="0")
        average.day$timeint<-as.duration(hm(sub("(..)$", ":\\1", time.padded2)))
        
        # 4. Time series plot of the average number of steps taken
        # Make a time series plot (i.e. `type = "l"`) of the 5-minute interval (x-axis) and 
        # the average number of steps taken, averaged across all days (y-axis)
        # Open png device, create 'plot1.png' in the working directory
        png(filename = "./figure/plot1.png",  width=640, height=480)
        
        # Make the plot with ggplot2. Time series should be in time format in order to plot correctly
        g2 <- ggplot(average.day, aes(as.duration(timeint), steps))
        g2 <- g2 + geom_line(size=1, color = "dodgerblue") +
                xlab("Time in the day (5 minute interval)") +
                ylab("Average number of Steps") +
                ggtitle("Daily activity pattern") + scale_x_time()
        
        print(g2)
        
        # Close the png file device
        dev.off()
        
        # 5. The 5-minute interval that, on average, contains the maximum number of steps
        # Which 5-minute interval, on average across all the days in the dataset, 
        # contains the maximum number of steps?
        maxSteps <- which.max(average.day$steps)
        maxtime <-average.day[maxSteps,"interval"]
        
        # Format interval column to time(h:m:s)
        time.padded3 <- str_pad(maxtime, 4, pad="0")
        maxtimeformat<-sub("(..)$", ":\\1", time.padded3)
        
## Question: Imputing missing values
        # 6. Code to describe and show a strategy for imputing missing data
        # Figuring out if there is some patters related to NA ocurrence
        # There is a total of 2304 NA in steps 
        sum(is.na(activity$steps))
        nas<-activity %>% group_by(date) %>% summarize(steps=sum(is.na(steps)))
        summary(nas)
        str(nas)
        
        # Every day should have 288 measures (60min * 24h / 5min interval)
        # It looks like there are 8 days with no measures at all. 
        # That´s 288 measures * 8 days = 2304 NAs. The mising dates are: 
        nas.days<-nas[which(nas$steps==288), "date"]
        
        # Open png device, create 'plot2.png' in the working directory
        png(filename = "./figure/plot2.png",  width=640, height=480)
        
        g3 <- ggplot(nas, aes(date, steps))
        g3 <- g3 + geom_line(size=1, color = "dodgerblue") + 
                xlab("Date") +
                ylab("Number of NAs") +
                ggtitle("NAs pattern in the dataset") + scale_x_date()
        
        print(g3)
        
        # Close the png file device
        dev.off()
        
        # Since in this case we know that the missing data appeared randomly in the
        # data, it would make sense to try to impute the values. Here, I use the Hmisc package 
        # to do a basic imputation of the missing data, by calculating the mean of the variable 
        # Create a new dataset that is equal to the original dataset but with the missing data filled in.
        library(Hmisc)
        activityImputed <- activity
        activityImputed$steps <- impute(activity$steps, fun=mean)
        
        # 7. Histogram of the total number of steps taken each day after missing values are imputed
        # Make a histogram of the total number of steps taken each day
        steps.day.imputed <- tapply(activityImputed$steps, activityImputed$date, sum)
        # Open png device, create 'hist2.png' in the working directory
        png(filename = "./figure/hist2.png",  width=640, height=480)
        
        g4<-qplot(steps.day.imputed, 
              xlab='Total steps per day', 
              ylab='Frequency',
              ylim = c(0,13),
              main = 'Total number of steps taken each day - Imputed dataset',
              fill=I("dodgerblue"), 
              col=I("blue"),
              binwidth=600)
        
        print(g4)
        
        # Close the png file device
        dev.off()
        detach("package:Hmisc", unload=TRUE)
        
        # Calculate and report the mean and median total number of steps taken per day. 
        # Do these values differ from the estimates 
        # from the first part of the assignment? What is the impact of imputing 
        # missing data on the estimates of the total daily number of steps?
        
        mean.steps.day.Im <- mean(steps.day.imputed)
        median.steps.day.Im <- median(steps.day.imputed)
        
## Question: Are there differences in activity patterns between weekdays and weekends?
        # 8. Panel plot comparing the average number of steps taken per 5-minute interval across 
        # weekdays and weekends
        # Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
        # indicating whether a given date is a weekday or weekend day.
        activityImputed$dayType <- ifelse(wday(activityImputed$date) %in% c(6,7), "weekend", "weekday") 
         
        # Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval 
        # (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days 
        # (y-axis). 
        average.day.Imp <- 
                activityImputed %>%
                group_by(interval,dayType) %>%
                summarize(steps=mean(steps))
        # Format interval column to time(h:m:s)
        time.padded4 <- str_pad(average.day.Imp$interval, 4, pad="0")
        average.day.Imp$timeint<-as.duration(hm(sub("(..)$", ":\\1", time.padded4)))
        
        # Open png device, create 'plot3.png' in the working directory
        png(filename = "./figure/plot3.png",  width=640, height=480)
        
        # Make the plot with ggplot2. Time series should be in time format in order to plot correctly
        g5 <- ggplot(average.day.Imp, aes(as.duration(timeint), steps))
        g5 <- g5 + geom_line(size=1, color = "dodgerblue") +
                facet_grid(dayType ~ .) +
                xlab("Time in the day (5 minute interval)") +
                ylab(expression("Average number of Steps")) +
                ggtitle("Daily activity pattern - weekdays vs weekend") + scale_x_time()
        
        print(g5)
        
        # Close the png file device
        dev.off()