steps <- aggregate(x=data$steps, by=list(data$date), FUN=sum, na.rm=T)
steps <- steps[!steps$x==0,]

hist(steps$x,breaks = 25, main = "Number of steps taken each day",
     xlab = "Number of Steps", ylab = "Number of days")

print(mean(steps$x))
print(median(steps$x))

steps <- aggregate(x=data$steps,by=list(data$interval),FUN=mean,na.rm = T)

plot(steps$Group.1,steps$x,main = "Average steps taken",xlab = "Interval"
     ,ylab = "Number of steps",type = "l")

print(steps[steps$x == max(steps$x),1])

print(sum(!complete.cases(data)))

steps$x = round(steps$x)

impute <- function(x){
  if(is.na(x[1])){
    x[1] <- steps[steps$Group.1 == as.numeric(x[3]),2]
  }
  x[1]
}
data2<-data
data2$steps <- as.numeric(apply(data,1,impute))

steps <- aggregate(x=data2$steps, by=list(data2$date), FUN=sum, na.rm=T)

hist(steps$x,breaks = 25, main = "Number of steps taken each day",
     xlab = "Number of Steps", ylab = "Number of days")

print(mean(steps$x))
print(median(steps$x))

type <- weekdays(data2$date)
type[type %in% c("Sunday","Saturday")] <- "weekend"
type[!type=="weekend"] <- "weekday"
type <- as.factor(type)
data2$type <- type

steps <- aggregate(steps~interval+type,data2,FUN=mean)

library(ggplot2)

print(ggplot(steps, aes(x=interval,y=steps))+geom_line(color = "steelblue")+
        facet_grid(type~.)+labs(y = "Number of Steps"))
