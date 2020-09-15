library("knitr")
#Load the data 
Data <- read.csv("activity.csv", header = TRUE)
head(Data)

#Calculate the total number of steps per a day and make a histogram 
steps_per_day <- Data %>%
      group_by(date) %>%
      summarize(sum_steps = sum(steps, na.rm = TRUE)) 

head(steps_per_day)
hist(steps_per_day$sum_steps, 
     main = "Histogram of the total number of steps per a day ", xlab="Steps")

#Calculate the mean and median of the total step number per a day 
pre_NA_mean_stpes_per_day <- mean(steps_per_day$sum_steps)
pre_NA_median_stpes_per_day <- median(steps_per_day$sum_steps)

#What is the average daily activity pattern?
#Time series plot of 5-mins interval (x-axis) and the average step number 
steps_interval <- Data %>%
      group_by(interval) %>%
      summarize(meansteps = mean(steps, na.rm = TRUE)) 

head(steps_interval)
plot(meansteps ~ interval, data = steps_interval, type="l",
     xlab = "5 minute intervals", ylab = "Average step number",
     main = "Steps by time interval")

#Calculate the interval that contains the most stpes on average 
steps_interval$interval[which.max(steps_interval$meansteps)]

#Calculate and report the total number of missing values in the dataset
sum(is.na(Data$steps))


#Create new data set to fill NA with the mean of 5 mins interval 
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
replace_na_by_mean <- Data%>% 
      group_by(interval) %>% 
      mutate(steps = replacewithmean(steps))
head(replace_na_by_mean)

#Make a histogram and calcuate the mean and median after replace NA
steps_per_day_replace_na_by_mean <- replace_na_by_mean %>%
      group_by(date) %>%
      summarize(sum_steps = sum(steps, na.rm = TRUE)) 
head(steps_per_day_replace_na_by_mean)
hist(steps_per_day_replace_na_by_mean$sum_steps, 
     main = "Histogram of total step numbers per a day ", xlab="Steps")

post_NA_mean_stpes_per_day<-mean(steps_per_day_replace_na_by_mean$sum_steps)
post_NA_median_stpes_per_day <- median(steps_per_day_replace_na_by_mean$sum_steps)

#Compare before vs after NA replace 
pre_NA_mean_stpes_per_day 
pre_NA_median_stpes_per_day
post_NA_mean_stpes_per_day
post_NA_median_stpes_per_day

#Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 
replace_na_by_mean$date <- as.Date(replace_na_by_mean$date)
replace_na_by_mean$weekday <- weekdays(replace_na_by_mean$date)
replace_na_by_mean$weekend <- ifelse(replace_na_by_mean$weekday=="Saturday"|replace_na_by_mean$weekday=="Sunday",
                                     "Weekend", "Weekday" )
as.factor(replace_na_by_mean$weekend)
head(replace_na_by_mean)
filtered_weekday <- filter(replace_na_by_mean, weekend == "Weekday")
filtered_weekend <- filter(replace_na_by_mean, weekend == "Weekend")


#Make plot
steps_interval_weekday <- filtered_weekday %>%
      group_by(interval) %>%
      summarize(meansteps = mean(steps, na.rm = TRUE)) 
steps_interval_weekday$day <- "weekday"
steps_interval_weekday

steps_interval_weekend <- filtered_weekend %>%
      group_by(interval) %>%
      summarize(meansteps = mean(steps, na.rm = TRUE)) 
steps_interval_weekend$day <- "weekend"
steps_interval_weekend

steps_interval_week_weekend <-rbind(steps_interval_weekday, steps_interval_weekend)
week_weekend <- ggplot (steps_interval_week_weekend, aes (interval, meansteps))+
      geom_line() + facet_grid (day~.) + 
      theme(axis.text = element_text(size = 12),axis.title = element_text(size = 14)) + 
      labs(y = "Step Number") + labs(x = "Interval") + 
      ggtitle("Average step Number")


knit2html("PA1_template.Rmd")