
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


