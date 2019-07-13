Project 1
================

### Loading libraries

``` r
library(ggplot2, quietly = TRUE)
library(dplyr,quietly = TRUE)
library(lubridate,quietly = TRUE)
library(VIM,quietly = TRUE)
```

### Loading and preprocessing the data

``` r
act <- data.table :: fread("./activity.csv")
```

### Converting date from chracter to Date class

``` r
act$date <- lubridate :: ymd(act$date)
```

What is mean total number of steps taken per day?
-------------------------------------------------

### Total steps per day

``` r
Total_steps <- aggregate(act$steps, FUN = sum, list(Day = act$date), na.rm=T)
```

### Plotting Hitogram for total daily steps

``` r
ggplot(Total_steps,aes(x)) + 
        geom_histogram(binwidth=2000 ,fill="red",
        colour="black", size=1) + labs(title="Histogram of Total Daily Steps",
        x="Total Daily Steps", y="Frequency")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-5-1.png)

### Calculating and reporting the mean and median of TotalDaily Steps

``` r
mean_1 <- mean(Total_steps$x)
median_1 <- median(Total_steps$x)
```

### Mean

``` r
mean_1
```

    ## [1] 9354.23

### Median

``` r
median_1
```

    ## [1] 10395

What is the average daily activity pattern?
-------------------------------------------

### Constructing a dataframe for Average Daily Activity Pattern

``` r
Avg_steps <- aggregate(act$steps,
        FUN = mean, list(Interval = act$interval), na.rm=T)
```

### Line plot for Average Daily Activity Pattern

``` r
ggplot(Avg_steps, aes(Interval, x)) + geom_line(colour="blue",size=1) +
    labs(title = "Average Daily Activity Pattern", y="Average number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-10-1.png)

### The 5-minute interval, on average across all the days in the dataset, that contains the maximum number of steps

``` r
Avg_steps$Interval[Avg_steps$x==max(Avg_steps$x)]
```

    ## [1] 835

Imputing missing values
-----------------------

### Number of "NA" present

``` r
length(act$steps[act$steps == "NA"])
```

    ## [1] 2304

### Creating a new dataframe by imputing missing values by K-Nearest Neighbour method

``` r
act_imputed <- kNN(act, variable="steps", k=5)
```

### Total steps per day (imputed)

``` r
Total_steps_imputed <- aggregate(act_imputed$steps, FUN = sum, list(Day = act_imputed$date), na.rm=T)
```

### Plotting Hitogram for total daily steps (imputed)

``` r
ggplot(Total_steps_imputed,aes(x)) + 
    geom_histogram(binwidth=2000 ,fill="red",
    colour="black", size=1) + labs(title="Histogram of Total Daily Steps (with imputed              data)",x="Total Daily Steps", y="Frequency")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-15-1.png)

### Calculating and reporting the mean and median of Total Daily Steps (Imputed)

``` r
mean_2 <- mean(Total_steps_imputed$x)
median_2 <- median(Total_steps_imputed$x)
```

#### Mean (imputed):

``` r
mean_2
```

    ## [1] 9752.393

#### Median (imputed):

``` r
median_2
```

    ## [1] 10395

### Difference in mean, mean(imputed) - mean(original)

``` r
mean_2 - mean_1
```

    ## [1] 398.1639

### Difference in median, median(imputed) - median(original)

median\_2-median\_1

### There is was no change in the mdian after imputation, but the mean increased by 398.1639

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

### Creating factor variables with two levels - weekday and weekend

``` r
act_imputed$Day_Factor <- ifelse(weekdays(act$date) %in% 
                c("Sunday","Saturday"), "weekend","weekday")
```

### Constructing a dataframe for Average Daily Activity Pattern for weekdays and weekends

``` r
Avg_steps_imputed_days <- aggregate(act_imputed$steps,
                            FUN = mean, list(Interval = act_imputed$interval,
                            Day=act_imputed$Day_Factor),  na.rm=T)
```

### Line plot for Average Daily Activity Pattern (with imputed steps) with breakdown in weekday and weekend

``` r
ggplot(Avg_steps_imputed_days, aes(Interval, x,colour=factor(Day))) + geom_line(size=1) +
    facet_wrap(~Day, ncol = 1, nrow=2)+
    labs(title = "Average Daily Activity Pattern (with imputed data)", 
    y="Average number of steps", color="Day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-22-1.png)
