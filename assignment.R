#read the data
activity<-read.csv("activity.csv")
library("ggplot2")
#remove na values
recorded<-subset(activity,!is.na(activity$steps))

#no of steps recorded each day
png("plot1.png")
g<-ggplot(data=recorded,aes(date,steps))+ geom_histogram(stat="identity")
g+theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()

#mean and median steps per day
mean_per_day<-tapply(recorded$steps,recorded$date,mean)
mean_per_day<-mean_per_day[!is.na(mean_per_day)]
mean_per_day
median_per_day<-tapply(recorded$steps,recorded$date,median)
median_per_day<-data.frame(median_per_day)
median_per_day

#time series plot
png("plot2.png")
plot(unique(recorded$date), mean_per_day,pch=20,type="1")
lines(unique(recorded$date), mean_per_day)
dev.off()

# max average
##the interval that contains highest avg no of steps.
steps_interval<-tapply(recorded$steps,recorded$interval,mean)
max<-steps_interval[steps_interval==max(steps_interval)]
max

#Imputing Data
## replace na by mean of that particular interval 
sum(is.na(activity$steps))
activity2<-activity
activity2[is.na(activity2$steps),"steps"]<-steps_interval[as.character(activity2[is.na(activity2$steps),"interval"])]

#mean and median steps per day
mean_per_day<-tapply(activity2$steps,activity2$date,mean)
mean_per_day<-data.frame(mean_per_day)
mean_per_day
median_per_day<-tapply(activity2$steps,activity2$date,median)
median_per_day<-data.frame(median_per_day)
median_per_day


#Recalculating steps per day histogram

png("plot3.png")
g<-ggplot(data=activity2,aes(date,steps))+ geom_histogram(stat="identity")
g+theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off()


#weekdays vs weekends

recorded["days"]<-weekdays(as.Date(recorded$date,"%Y-%m-%d"))
recorded[grepl("Monday|Tuesday|Wednesday|Thursday|Friday",recorded$days),"weekends"]<-"weekday"
recorded[grepl("Saturday|Sunday",recorded$days),"weekends"]<-"weekend"
recorded[,"weekends"]<-as.factor(recorded$weekends)

png("plot4.png")
ggplot(data=recorded,aes(interval,steps))+geom_line()+facet_grid(.~weekends)
dev.off()
  
  


