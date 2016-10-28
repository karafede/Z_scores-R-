# R padding time series with missing time units

# Import the dataset with wholes in the timeline - date not complete

wind <-  read.csv("H:/Stansted2016/test.csv", header = TRUE)

# Transform date into date format
library(lubridate)
library(dplyr)

wind <- wind %>%
  mutate(date = dmy_hm(date, tz="UTC"))

# If date is set as character, use this command to turn it into a date format 
#- very important as this can block the construction of any chart, timeplot, etc
#str(wind)
#wind$date <- as.Date (wind$date)



sorted.data <- wind[order(wind$date),]

data.length <- length(sorted.data$date)

timemin <- sorted.data$date[1]
timemax <- sorted.data$date[data.length]

# Command line to eliminate the NAs of a timeseries
#sd_volatile <- sd_volatile[!is.na(sd_volatile$v25),] 

all.dates <- seq(timemin, timemax, by = "min") # This command only works with date format as POSIXT
all.dates <- as.data.frame(all.dates)
colnames(all.dates) <- "date"


merged.data <- merge(all.dates, sorted.data, all=T)

write.csv(merged.data,file="H:/Stansted2016/wind.csv", row.names = FALSE)




