# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

```{r}

activitydata <- read.csv("C:/Users/Deepthi M N/Documents/GitHub/RepData_PeerAssessment1/activity.csv")

```



## What is mean total number of steps taken per day?

### Plotting histogram and distribution of total steps taken each day

```{r}

activitydata2 <- aggregate(steps~date, sum, data=activitydata, na.rm = FALSE)
hist(activitydata2$steps, main = "Histogram of steps taken each day", xlab = "Number of steps")
barplot(activitydata2$steps, main = "Distribution of steps taken by date", xlab = "Days", ylab = "Number of steps")

```

### Calculating mean of steps
```{r}

avgsteps <- mean(activitydata2$steps, na.rm = FALSE)
avgsteps

```

### Calculating median of steps

```{r}

mediansteps <- median(activitydata2$steps, na.rm = FALSE)
mediansteps

```



## What is the average daily activity pattern?

### Time series plot

```{r}

activitydata3 <- aggregate(steps~interval, mean, data=activitydata,na.rm=TRUE)
plot(activitydata3, type = "l", main = "Average number of steps taken by interval", xlab = "Interval" , ylab = "Avg. Number of steps taken" )

```

### 5 minute interval with maximum average number of steps

```{r}

maxstep <- max(activitydata3$steps)
max5int <- activitydata3$interval[which(activitydata3$steps == maxstep)]
max5int

```



## Imputing missing values

### Computing the number of rows with NA

```{r}

activitydata_nona <- apply(activitydata, 1, function(x){any(is.na(x))})
sum(activitydata_nona)

```

### Missing value treatment: Replacing missing value with interval mean (Points 2 and 3 covered)

```{r}

activitydata4 <- activitydata
activitydata4 <- merge(x=activitydata, y=activitydata3, by="interval", all.x = TRUE,suffixes = c(".Old",".New"))
colnames(activitydata4) <- c("interval", "steps", "date", "avgsteps")
activitydata4$stepsbu <- activitydata4$steps        

for(i in 1:nrow(activitydata4))
        {if(is.na(activitydata4$steps[i])== TRUE)
                {activitydata4$steps[i] <- activitydata4$avgsteps[i] }
         else activitydata4$steps[i] <- activitydata4$steps[i]
}

##activitydata4

```


### Plotting histogram using missing value treated data

```{r}

activitydata5 <- aggregate(steps~date, sum, data=activitydata4)
hist(activitydata5$steps, main = "Histogram of steps taken each day (No NA)", xlab = "Number of steps")

```

### Calculating mean of steps using missing value treated data
```{r}

avgsteps <- mean(activitydata5$steps)
avgsteps

```

### Calculating median of steps using missing value treated data

Median calculated without NA is smaller than that calulated using data with NA. 
```{r}

mediansteps <- median(activitydata5$steps)
mediansteps

```



## Are there differences in activity patterns between weekdays and weekends?

### Creating weekday/weekend factor

```{r}

activitydata6 <- activitydata4
activitydata6$day <- weekdays(as.Date(activitydata6$date))

day <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
daycat <- c("Weekend", "Weekday", "Weekday", "Weekday", "Weekday", "Weekday", "Weekend")
key <- data.frame(day,daycat)

activitydata6   <- merge(x=activitydata6, y=key, by="day", all.x = TRUE,suffixes = c(".Old",".New"))

##write.table(activitydata6, "C:/Users/Deepthi M N/Desktop/Coursera/Course 5 - Reproducible Research - Assignment 1/mydata.txt", sep="\t")


```


### Creating panel plot

```{r}

activitydata7 <- aggregate(steps~interval+daycat, data = activitydata6, mean, na.rm=TRUE)

library(lattice)

xyplot(steps ~ interval|daycat, data=activitydata7, type='l', layout = c(1,2), main = "Avg. steps by weekday/weekend", xlab = "Interval", ylab = "Avg. number of steps")


```



