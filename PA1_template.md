\#\#Loading and processing data

-Load the data -Process/transform the data (if necessary) into a format
suitable for your analisys.

``` r
#loading librarys
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

``` r
##loading data
fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(fileUrl,destfile="./activity.zip",method="curl")
unzip("activity.zip")
activity_steps <- read.csv("activity.csv", header = TRUE)
activity_steps$date <- as.Date(activity_steps$date)
head(activity_steps)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?
-------------------------------------------------

For this part of the assignment, you can ignore the missing values in
the dataset.

1.Calculate the total number of steps taken per day

``` r
##total number of steps
Steps_day <- activity_steps %>% group_by(date) %>% summarize(sumsteps_day = sum(steps, na.rm = TRUE))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
head(Steps_day)
```

    ## # A tibble: 6 x 2
    ##   date       sumsteps_day
    ##   <date>            <int>
    ## 1 2012-10-01            0
    ## 2 2012-10-02          126
    ## 3 2012-10-03        11352
    ## 4 2012-10-04        12116
    ## 5 2012-10-05        13294
    ## 6 2012-10-06        15420

2.Make a histogram of the total number of steps taken each day

``` r
##histogram
hist(Steps_day$sumsteps_day, main = "Steps Per Day", col= "red", xlab="Steps")
```

![](PA1_template_files/figure-markdown_github/histogram-1.png)

3.Calculate and report the mean and median of the total number of steps
taken per day.

``` r
##mean and median
mean(Steps_day$sumsteps_day)
```

    ## [1] 9354.23

``` r
median(Steps_day$sumsteps_day)
```

    ## [1] 10395

\#\#What is the average daily activity pattern?

Make a time series plot of the 5-minute interval (x-axis) and the
average number of steps taken, averaged across all days (y-axis)

``` r
library(ggplot2)
steps_Interval <- activity_steps %>% group_by(interval) %>% summarize(mean_steps = mean(steps, na.rm = TRUE)) 
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
ggplot(steps_Interval, aes(x=interval, y=mean_steps))+ geom_line()+labs(x= "5 Minute Intervals", y="Average Number of Steps", title = "Steps By 5 Minute Interval") 
```

![](PA1_template_files/figure-markdown_github/time%20series%20plot-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

``` r
steps_Interval[which(steps_Interval$mean_steps == max(steps_Interval$mean_steps)),]
```

    ## # A tibble: 1 x 2
    ##   interval mean_steps
    ##      <int>      <dbl>
    ## 1      835       206.

\#\#Imputing missing values

1.Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)

``` r
missing_values <- sum(is.na(activity_steps))
print(missing_values)
```

    ## [1] 2304

2.Devise a strategy for filling in all of the missing values in the
dataset. The strategy does not need to be sophisticated. For example,
you could use the mean/median for that day, or the mean for that
5-minute interval, etc.

``` r
library(magrittr)
library(dplyr)

replace_missingValues<- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
mean_interval <- activity_steps%>% group_by(interval) %>% mutate(steps= replace_missingValues(steps))
head(mean_interval, 10)
```

    ## # A tibble: 10 x 3
    ## # Groups:   interval [10]
    ##     steps date       interval
    ##     <dbl> <date>        <int>
    ##  1 1.72   2012-10-01        0
    ##  2 0.340  2012-10-01        5
    ##  3 0.132  2012-10-01       10
    ##  4 0.151  2012-10-01       15
    ##  5 0.0755 2012-10-01       20
    ##  6 2.09   2012-10-01       25
    ##  7 0.528  2012-10-01       30
    ##  8 0.868  2012-10-01       35
    ##  9 0      2012-10-01       40
    ## 10 1.47   2012-10-01       45

3.Create a new dataset that is equal to the original dataset but with
the missing data filled in.

``` r
head(mean_interval, 10)
```

    ## # A tibble: 10 x 3
    ## # Groups:   interval [10]
    ##     steps date       interval
    ##     <dbl> <date>        <int>
    ##  1 1.72   2012-10-01        0
    ##  2 0.340  2012-10-01        5
    ##  3 0.132  2012-10-01       10
    ##  4 0.151  2012-10-01       15
    ##  5 0.0755 2012-10-01       20
    ##  6 2.09   2012-10-01       25
    ##  7 0.528  2012-10-01       30
    ##  8 0.868  2012-10-01       35
    ##  9 0      2012-10-01       40
    ## 10 1.47   2012-10-01       45

4.Make a histogram of the total number of steps taken each day and
Calculate and report the mean and median total number of steps taken per
day. Do these values differ from the estimates from the first part of
the assignment? What is the impact of imputing missing data on the
estimates of the total daily number of steps?

``` r
Steps_day2 <- mean_interval %>% group_by(date) %>% summarize(sum_steps2=sum(steps))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
hist(Steps_day2$sum_steps2, main= "Steps per day", col = "pink")
```

![](PA1_template_files/figure-markdown_github/histogram2-1.png)

``` r
##mean and median- old data 
mean(Steps_day$sumsteps_day)
```

    ## [1] 9354.23

``` r
median(Steps_day$sumsteps_day)
```

    ## [1] 10395

``` r
##mean and median new data
mean(Steps_day2$sum_steps2)
```

    ## [1] 10766.19

``` r
median(Steps_day2$sum_steps2)
```

    ## [1] 10766.19

yes, there are some changes , the mean and the median increases related
with the old data

\#\#Are there differences in activity patterns between weekdays and
weekends?

1.Create a new factor variable in the dataset with two levels –
“weekday” and “weekend” indicating whether a given date is a weekday or
weekend day.

``` r
mean_interval$date <- as.Date(mean_interval$date)
mean_interval$weekday <- weekdays(mean_interval$date)
mean_interval_weekend <- ifelse(mean_interval$weekday=="Saturday" | mean_interval$weekday=="Sunday", "Weekend", "Weekday" )
head(mean_interval)
```

    ## # A tibble: 6 x 4
    ## # Groups:   interval [6]
    ##    steps date       interval weekday
    ##    <dbl> <date>        <int> <chr>  
    ## 1 1.72   2012-10-01        0 lunes  
    ## 2 0.340  2012-10-01        5 lunes  
    ## 3 0.132  2012-10-01       10 lunes  
    ## 4 0.151  2012-10-01       15 lunes  
    ## 5 0.0755 2012-10-01       20 lunes  
    ## 6 2.09   2012-10-01       25 lunes

2.Make a panel plot containing a time series plot (i.e. type = “l”) of
the 5-minute interval (x-axis) and the average number of steps taken,
averaged across all weekday days or weekend days (y-axis). See the
README file in the GitHub repository to see an example of what this plot
should look like using simulated data.

``` r
New_interval <- mean_interval %>% group_by(interval, weekday)  %>% summarise(meanSteps = mean(steps, 
    na.rm = TRUE))
```

    ## `summarise()` regrouping output by 'interval' (override with `.groups` argument)

``` r
ggplot(data = New_interval, aes(x = interval, y = meanSteps)) + geom_line() + facet_grid(weekday~.) +  xlab("Interval") + ylab("Mean of Steps") + ggtitle("Comparison of Average Number of Steps in Each Interval")
```

![](PA1_template_files/figure-markdown_github/plot-1.png)
