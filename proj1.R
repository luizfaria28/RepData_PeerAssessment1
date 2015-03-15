
# 1
df <- read.csv("activity.csv")

# 2
library(plyr)
df2_complete <- df[complete.cases(df),c(2,1,3)]
df2 <- ddply(df2_complete[,c(1,2)], .(date), 
      summarize, 
      steps = sum(steps))
hist(df2$steps, main="Total number of steps taken each day", xlab="Steps")
print(mean(df2$steps))
print(median(df2$steps))

# 3
df3 <- ddply(df2_complete[,c(2,3)], .(interval), 
             summarize, 
             steps = mean(steps))
plot(df3, type="l")
row <- which.max(df3[,2])
df3[row,1] # interval

# 4
# the total number of missing values in the dataset (i.e. the total number of rows with NAs)
print(sum(!complete.cases(df)))
# Filling in all of the missing values in the dataset using the mean of steps for that 5-minute interval
df4 <- df
ind <- !complete.cases(df)
# Para cada ind, ir buscar a df3 o valor de steps$df3 cujo interval$df3 == df[ind$]
# df4$steps[ind] = df3$steps[df3$interval == 5]
getSteps <- function(intval) df3$steps[df3$interval == intval] 
listSteps <- lapply(df4$interval[ind], getSteps)
# Create a new dataset that is equal to the original dataset but with the missing data filled in
df4$steps[ind] <- listSteps
df4$steps <- as.numeric(df4$steps)
df5 <- ddply(df4[,c(2,1)], .(date), 
             summarize, 
             steps = sum(steps))
hist(df5$steps, main="Total number of steps taken each day", xlab="Steps")
print(mean(df5$steps))
print(median(df5$steps))

# 5
df4$day <- ifelse(weekdays(as.Date(df4$date)) == "Saturday" | 
                    weekdays(as.Date(df4$date)) == "Sunday", "Weekend", "Weekday")
df4$day <- as.factor(df4$day)

df4Weekend <- ddply(df4[df4$day == "Weekend",c(1,3)], .(interval), 
                    summarize, 
                    steps = mean(steps))    
df4Weekday <- ddply(df4[df4$day == "Weekday",c(1,3)], .(interval), 
                    summarize, 
                    steps = mean(steps))

par(mfrow=c(2,1))
plot(df4Weekday, type="l", main="Weekdays")
plot(df4Weekend, type="l", main="Weekends")


