---
title: "Reproducible Research - Project 1"
author: "Shane L Hubler, PhD"
date: "Tuesday, August 12, 2014"
output: html_document
---

# 1 - Preliminaries
### Constant(s) used later:




```r
# Note that this address has been altered: the "s" was removed from "https"
FILE_HTTP = "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# Assumes environment is using English
LIST_OF_WEEKDAYS = c("Monday","Tuesday","Wednesday","Thursday","Friday")
```

### Utility functions:

```r
convert_to_datetime <- function(date.str, interval) {
    interval.str = sprintf("%04i", interval)
    datetime.str = paste(date.str,interval.str)
    datetime = as.POSIXct(datetime.str, format="%Y-%m-%d %H%M",tz="UTC")
    return(datetime)
}

convert_to_weekday_weekend <- function(dt) {
    day = weekdays(as.Date(dt[[1]]))
    if(day %in% LIST_OF_WEEKDAYS) {"weekday"}
    else {"weekend"}
}

plot_steps_vs_time <- function(data,day_type,max_steps){
    m = data[data$daytype == day_type,]
    with(m, plot(time,
                 steps,
                 type="l",
                 main=day_type,
                 xlab = "time",
                 ylab = "mean steps",
                 ylim=c(0,max_steps)
                 )
         )
}
```

### Loading
Data is downloaded using from the link provided in the project description.
The data is loaded directly into memory - the file is not saved on disk.


```r
temp = tempfile()
download.file(url = FILE_HTTP, destfile = temp)
data = read.csv(unz(temp,"activity.csv"))
unlink(temp)
#setwd(DATA_DIR)
```

### Preprocessing
*date* and *interval* are combined into *datetime* (POSIXct format).
The new *time* variable is derived from *datetime*.
Note that the column labeled *interval* is a time formatted as HHMM, which
makes it awkward for plotting and requiring more complex handling to avoid
spurious flat sections (e.g. between 155 and 200).


```r
data$datetime = mapply(convert_to_datetime,data$date,data$interval,SIMPLIFY=FALSE)

data$time = unlist(mapply(function(a){unlist(format(a,"%H:%M"))}, data$datetime, SIMPLIFY=FALSE))
data$time = as.POSIXct(data$time, format="%H:%M")
#I would love to know a simple way to avoid attaching a date to this value...
```

# 2 - Steps per day


```r
steps.per.day = aggregate(steps~date, data, sum)
hist(steps.per.day$steps,
     breaks = 10,
     xlab="Total steps",
     ylab="Frequency (days)",
     main="Histogram of steps per day"
     )
```

![plot of chunk Steps_per_day](figure/Steps_per_day.png) 


```r
steps.per.day.mean = mean(steps.per.day$steps)
steps.per.day.median = median(steps.per.day$steps)
display.mean = sprintf("%.1f", steps.per.day.mean)
display.median = sprintf("%.1f", steps.per.day.median)
```

The mean and median steps per day, respectively, are


```r
display.mean
```

```
## [1] "10766.2"
```


```r
display.median
```

```
## [1] "10765.0"
```

# 3 - Daily pattern


```r
steps.per.interval = aggregate(steps~time, data, mean, na.rm=TRUE)
with(steps.per.interval,
     plot(steps~time, 
          main="Mean number of steps per 5-minute interval",
          type = "l")
     )
```

![plot of chunk Daily_pattern](figure/Daily_pattern.png) 

The time with the greatest mean number of steps is


```r
idx = with(steps.per.interval, 
           which(steps == max(steps,na.rm = rm),)
           )
max.interval = steps.per.interval[idx,]

format(max.interval$time,"%H:%M")
```

```
## [1] "08:35"
```

# 4 - Imputing missing values

The number of missing values:

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Here we create a new column, *imputed.steps*, that replaces all NA's with the
mean for that interval, as computed in the previous section. Note that I have
decided, for the sake of efficiency and clarity, to ignore the "new dataset" 
requirement in condition 3 of "Imputing missing values".  The "new dataset"
effectively exists as a new (single) column in the original dataframe.


```r
na.idx = which(is.na(data$steps))
imputed = data[na.idx,]
imputed$steps = steps.per.interval$steps[match(imputed$time, 
                                               steps.per.interval$time)]
data$imputed.steps         = data$steps     #Default to actual value
data$imputed.steps[na.idx] = imputed$steps  #Replace na's
```

### The effects of imputation

```r
imputed.steps.per.day = aggregate(imputed.steps~date, data, sum)
hist(imputed.steps.per.day$imputed.steps,
     breaks = 10,
     main = "Histogram of steps per day (includes imputed data)",
     xlab = "Total steps",
     ylab = "Frequency (days)"
     )
```

![plot of chunk Steps_per_day_imputed](figure/Steps_per_day_imputed.png) 


```r
imputed.steps.per.day.mean = mean(imputed.steps.per.day$imputed.steps)
imputed.steps.per.day.median = median(imputed.steps.per.day$imputed.steps)
imputed.display.mean = sprintf("%.1f", imputed.steps.per.day.mean)
imputed.display.median = sprintf("%.1f", imputed.steps.per.day.median)
```

The mean and median steps per day, respectively, are


```r
imputed.display.mean
```

```
## [1] "10766.2"
```


```r
imputed.display.median
```

```
## [1] "10766.2"
```

The main effect of this imputation method is that there are now a greater number
of "average" days.  This forces the overall mean to become closer to the median.
Note that the histogram has an identical shape except for the middle-most 
grouping, which has a larger number of days included.

One other odd effect is that the median is no longer a factor of one-half, as
might be expected for the median of integers (steps).  

These facts are easily explained by recognizing that all of the missing data 
occurs in blocks that include an entire day.  In other words, we are literally 
replacing the missing days with an "average" day, which, incidentally, does not
have an integer number of steps (similar to the "each family has 1.8 children" 
conundrum).

# 5 - Weekdays vs. Weekends


```r
data$daytype = factor(sapply(data$datetime,convert_to_weekday_weekend))

ag = aggregate(steps~time+daytype,data,mean)

#Calculating overall max so that the two plots are on the same scale.
max_steps = max(ag$steps)

#Plotting
par(mfrow = c(2,1))
# I used a specialized plotting function to ensure identical treatment
# of the two states ("weekend" and "weekday").
# Also, this makes it clear which plot is which.
# See Utility Functions for the function definition.
plot_steps_vs_time(data = ag, day_type = "weekday", max_steps = max_steps)
plot_steps_vs_time(data = ag, day_type = "weekend", max_steps = max_steps)
```

![plot of chunk Making_weekends](figure/Making_weekends.png) 

