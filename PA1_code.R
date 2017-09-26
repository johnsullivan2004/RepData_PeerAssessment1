## Source code for RepData Peer Assessment by JAS 9/24/2017

# Libraries
library(dplyr)
library(lattice)

# Unzip the data
unzip ("./activity.zip", exdir = ".")
# Read the data
activity_data <- read.csv("./activity.csv")


# Summarize by day
activity_daily <- activity_data %>%
    ## Group by day
    group_by(date)  %>% 
    ## Summarize
    summarise(Daily_Steps = sum(steps))

    
hist(activity_daily$Daily_Steps, col="green",breaks = 10, main="Total Steps per Day",
     xlab = "Steps",ylab="Days")

mean_steps <- mean(activity_daily$Daily_Steps,na.rm = TRUE)
median_steps <- median(activity_daily$Daily_Steps,na.rm = TRUE)

abline (v = mean_steps, col="blue", lwd=2,lty="dotted")
text (x=(mean_steps + 300), y=12,"mean",adj = c(0,0))

activity_interval <- activity_data %>%
    ## Group by day
    group_by(interval)  %>% 
    ## Summarize
    summarise(Interval_Steps = mean(steps,na.rm=TRUE))

plot(activity_interval$interval,activity_interval$Interval_Steps,
        type = 'l', col="Blue", lwd=2, main="Average (Mean) Steps per Interval",
        xlab="5 minute Interval", ylab="Mean Steps")

max_interval_table <- filter(activity_interval, Interval_Steps == max(Interval_Steps))
max_interval <- as.numeric(max_interval_table[1,1])
max_interval_steps <- as.numeric(max_interval_table[1,2])

# Impute missing intervals from average of entire data set
activity_data_imp <- activity_data %>%
    ## Join in the average per interval
    left_join (activity_interval,by="interval") %>%
    ## Replace steps with rounded interval average if it is missing
    mutate(steps = case_when(is.na(.$steps) ~ as.integer(round(.$Interval_Steps,0)), !is.na(.$steps) ~ .$steps, TRUE ~ 0L)) %>%
    ## Drop the Interval_Steps
    select (-Interval_Steps)

# Create a second daily grouping with imputed data
activity_daily_2 <- activity_data_imp %>%
    ## Group by day
    group_by(date)  %>% 
    ## Summarize
    summarise(Daily_Steps = sum(steps))


# Create Histogram for this revised daily
hist(activity_daily_2$Daily_Steps, col="grey",breaks = 10, main="Total Steps per Day with Imputations",
     xlab = "Steps",ylab="Days")

mean_steps_2 <- mean(activity_daily_2$Daily_Steps,na.rm = TRUE)
median_steps_2 <- median(activity_daily_2$Daily_Steps,na.rm = TRUE)

abline (v = mean_steps_2, col="blue", lwd=2,lty="dotted")
text (x=(mean_steps_2 + 300), y=12,"mean",adj = c(0,0))

# Add the day_type variable
activity_data_imp_wkdy <- activity_data_imp %>%
    mutate(weekday = weekdays(as.Date(date))) %>%
    mutate(day_type = as.factor(case_when(.$weekday %in% c("Saturday","Sunday") ~ "Weekend", TRUE ~ "Weekday" )))

# Summarize by interal and day_type
activity_interval_wkdy <- activity_data_imp_wkdy  %>%
    ## Group by day
    group_by(interval,day_type)  %>% 
    ## Summarize
    summarise(Interval_Steps = mean(steps,na.rm=TRUE))

#panel plot in lattice
xyplot (Interval_Steps ~ interval | day_type, data=activity_interval_wkdy, type="l", 
        lwd=2, col.line="blue", ylab="Average Steps for Interval", xlab="5 Minute Interval",
        main="Step / Interval Time Series for Weekend vs. Weekdays",layout=c(1,2))




