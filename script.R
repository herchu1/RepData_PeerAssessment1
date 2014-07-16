
activity <- read.csv(unz('activity.zip', 'activity.csv'), stringsAsFactors = F)
activity$date <- as.Date(activity$date)
stepsbydate <- aggregate(steps ~ date, data=activity, FUN=sum)

library(ggplot2)
ggplot(stepsbydate, aes(x=date, y=steps)) + geom_histogram(alpha=.5, stat="identity")
mean(stepsbydate$steps, na.rm=T)
median(stepsbydate$steps, na.rm=T)


stepsbyinterval <- aggregate(steps ~ interval, data=activity, FUN=mean)
ggplot(stepsbyinterval, aes(x=interval, y=steps)) + geom_line()
maxrow <- stepsbyinterval$steps == max(stepsbyinterval$steps)
stepsbyinterval[maxrow,]$interval

sum(is.na(activity$steps))
activitynona <- merge(activity, stepsbyinterval, 'interval')
nasteps <- is.na(activitynona$steps.x)
activitynona$steps.x[nasteps] <- activitynona$steps.y[nasteps]
colnames(activitynona)[2] <- 'steps'

stepsbydatenona <- aggregate(steps ~ date, data=activitynona, FUN=sum)
ggplot(stepsbydatenona, aes(x=date, y=steps)) + geom_histogram(alpha=.5, stat="identity")
mean(stepsbydatenona$steps)
median(stepsbydatenona$steps)


isweekend <- weekdays(activitynona$date, abbreviate=T) %in% c('Sat','Sun')
activitynona$daytype[isweekend] <- 'weekend'
activitynona$daytype[!isweekend] <- 'weekday'
activitynona$daytype <- as.factor(activitynona$daytype)

stepsbyintervalnona <- aggregate(steps ~ interval + daytype, data=activitynona, FUN=mean)
ggplot(stepsbyintervalnona, aes(x=interval, y=steps)) + geom_line() + facet_grid(daytype ~ .)

