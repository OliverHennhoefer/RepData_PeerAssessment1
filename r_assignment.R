data <- read.csv("./activity.csv", header = TRUE, sep = ",", colClasses = c("numeric", "Date", "numeric"))

activity <- na.omit(data)
head(activity)

#Total steps per day
steps_per_day <- aggregate(activity$steps, by=list(activity$date), FUN = sum)
colnames(steps_per_day) <- c("date", "step_count")

hist(steps_per_day$step_count, breaks = 10)

mean <- mean(steps_per_day$step_count)
median <- median(steps_per_day$step_count)

#Time-Series
activity_daily_mean <- aggregate(activity$steps, by=list(activity$interval), FUN = mean)
colnames(activity_daily_mean) <- c("interval", "mean")
plot(x = activity_daily_mean$interval, y = activity_daily_mean$mean, type = "l")

max_int <- activity_daily_mean[which.max(activity_daily_mean$mean),]

#Imputing missing values

na_count <- sum(!complete.cases(data))
data$steps[is.na(data$steps)] <- mean(data$steps, na.rm = T)
new_count <- sum(!complete.cases(data))

steps_per_day2 <- aggregate(data$steps, by=list(data$date), FUN = sum)
colnames(steps_per_day2) <- c("date", "step_count")

hist(steps_per_day2$step_count, breaks = 10)

mean2 <- mean(steps_per_day2$step_count)
median2 <- median(steps_per_day2$step_count)

data$w_day <- weekdays(data$date)
data$day_type <- ifelse((data$w_day == "Samstag" | data$w_day == "Sonntag"), yes = "weekend", no = "weekday")

daily_weekday_mean <- subset(data, day_type == "weekday")
daily_weekend_mean <- subset(data, day_type == "weekend")

daily_weekday_mean <- aggregate(daily_weekday_mean$steps, by = list(daily_weekday_mean$interval), FUN = mean)
colnames(daily_weekday_mean) <- c("interval", "step_count")

daily_weekend_mean <- aggregate(daily_weekend_mean$steps, by = list(daily_weekend_mean$interval), FUN = mean)
colnames(daily_weekend_mean) <- c("interval", "step_count")

par(mfrow=c(1,2))
plot(x = daily_weekday_mean$interval, y = daily_weekday_mean$mean, type = "l", col = "red")
plot(x = daily_weekend_mean$interval, y = daily_weekend_mean$mean, type = "l", col = "blue")
