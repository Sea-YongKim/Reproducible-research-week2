#Reproducible research: Course project 


```r
library(tidyverse)
library(ggplot2)
```

Load the data 

```r
Data <- read.csv("activity.csv", header = TRUE)
head(Data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

Calculate the total number of steps per a day and make a histogram 

```r
steps_per_day <- Data %>%
      group_by(date) %>%
      summarize(sum_steps = sum(steps, na.rm = TRUE)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(steps_per_day$sum_steps, 
     main = "Histogram of the total number of steps per a day ", xlab="Steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-15-1.png" width="672" />

Calculate the mean and median of the total step number per a day 

```r
pre_NA_mean_stpes_per_day <- mean(steps_per_day$sum_steps)
pre_NA_median_stpes_per_day <- median(steps_per_day$sum_steps)
print(paste("The mean is: ", pre_NA_mean_stpes_per_day))
```

```
## [1] "The mean is:  9354.22950819672"
```

```r
print(paste("The median is: ", pre_NA_median_stpes_per_day))
```

```
## [1] "The median is:  10395"
```

What is the average daily activity pattern?
Time series plot of 5-mins interval (x-axis) and the average step number 

```r
steps_interval <- Data %>%
      group_by(interval) %>%
      summarize(meansteps = mean(steps, na.rm = TRUE)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
plot(meansteps ~ interval, data = steps_interval, type="l",
     xlab = "5 minute intervals", ylab = "Average step number",
     main = "Steps by time interval")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-17-1.png" width="672" />

Calculate the interval that contains the most stpes on average 

```r
steps_interval$interval[which.max(steps_interval$meansteps)]
```

```
## [1] 835
```

```r
print(paste("Interval containing the most steps on average: ",
            steps_interval$interval[which.max(steps_interval$meansteps)]))
```

```
## [1] "Interval containing the most steps on average:  835"
```

Calculate and report the total number of missing values in the dataset

```r
sum(is.na(Data$steps))
```

```
## [1] 2304
```

```r
print(paste("The total number of rows with NA is: ",sum(is.na(Data$steps))))
```

```
## [1] "The total number of rows with NA is:  2304"
```

Create new data set to fill NA with the mean of 5 mins interval 

```r
replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
replace_na_by_mean <- Data%>% 
      group_by(interval) %>% 
      mutate(steps = replacewithmean(steps))
head(replace_na_by_mean)
```

```
## # A tibble: 6 x 3
## # Groups:   interval [6]
##    steps date       interval
##    <dbl> <fct>         <int>
## 1 1.72   2012-10-01        0
## 2 0.340  2012-10-01        5
## 3 0.132  2012-10-01       10
## 4 0.151  2012-10-01       15
## 5 0.0755 2012-10-01       20
## 6 2.09   2012-10-01       25
```

```r
summary(replace_na_by_mean)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 27.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##                   (Other)   :15840
```

Make a histogram and calcuate the mean and median after replace NA

```r
steps_per_day_replace_na_by_mean <- replace_na_by_mean %>%
      group_by(date) %>%
      summarize(sum_steps = sum(steps, na.rm = TRUE)) 
```

```
## `summarise()` ungrouping output (override with `.groups` argument)
```

```r
hist(steps_per_day_replace_na_by_mean$sum_steps, 
     main = "Histogram of total step numbers per a day ", xlab="Steps")
```

<img src="PA1_template_files/figure-html/unnamed-chunk-21-1.png" width="672" />

```r
post_NA_mean_stpes_per_day<-mean(steps_per_day_replace_na_by_mean$sum_steps)
post_NA_median_stpes_per_day <- median(steps_per_day_replace_na_by_mean$sum_steps)

print(paste("The mean after na replacement is: ", post_NA_mean_stpes_per_day))
```

```
## [1] "The mean after na replacement is:  10766.1886792453"
```

```r
print(paste("The median after na replacement is: ", post_NA_median_stpes_per_day))
```

```
## [1] "The median after na replacement is:  10766.1886792453"
```

Compare before vs after NA replace 

```r
print(paste("pre_mean is: ",pre_NA_mean_stpes_per_day))
```

```
## [1] "pre_mean is:  9354.22950819672"
```

```r
print(paste("post_mean is:", post_NA_mean_stpes_per_day))
```

```
## [1] "post_mean is: 10766.1886792453"
```

```r
print(paste("pre_median is: ",pre_NA_median_stpes_per_day))
```

```
## [1] "pre_median is:  10395"
```

```r
print(paste("post_median is:", post_NA_median_stpes_per_day))
```

```
## [1] "post_median is: 10766.1886792453"
```

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” 

```r
replace_na_by_mean$date <- as.Date(replace_na_by_mean$date)
replace_na_by_mean$weekday <- weekdays(replace_na_by_mean$date)
replace_na_by_mean$weekend <- ifelse(replace_na_by_mean$weekday=="Saturday"|replace_na_by_mean$weekday=="Sunday",
                                     "Weekend", "Weekday" )
filtered_weekday <- filter(replace_na_by_mean, weekend == "Weekday")
filtered_weekend <- filter(replace_na_by_mean, weekend == "Weekend")
head(filtered_weekday)
```

```
## # A tibble: 6 x 5
## # Groups:   interval [6]
##    steps date       interval weekday weekend
##    <dbl> <date>        <int> <chr>   <chr>  
## 1 1.72   2012-10-01        0 Monday  Weekday
## 2 0.340  2012-10-01        5 Monday  Weekday
## 3 0.132  2012-10-01       10 Monday  Weekday
## 4 0.151  2012-10-01       15 Monday  Weekday
## 5 0.0755 2012-10-01       20 Monday  Weekday
## 6 2.09   2012-10-01       25 Monday  Weekday
```

```r
head(filtered_weekend)
```

```
## # A tibble: 6 x 5
## # Groups:   interval [6]
##   steps date       interval weekday  weekend
##   <dbl> <date>        <int> <chr>    <chr>  
## 1     0 2012-10-06        0 Saturday Weekend
## 2     0 2012-10-06        5 Saturday Weekend
## 3     0 2012-10-06       10 Saturday Weekend
## 4     0 2012-10-06       15 Saturday Weekend
## 5     0 2012-10-06       20 Saturday Weekend
## 6     0 2012-10-06       25 Saturday Weekend
```

Make plot


