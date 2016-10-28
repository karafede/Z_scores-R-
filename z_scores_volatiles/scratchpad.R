library(dplyr)
library(tidyr)
library(lubridate)

# Load data
data_db <- read.csv("C:/volatiles/v10_and_v25_daily_means_for_92_sites_2015-07-01_to_2015-09-30.csv")
head(data_db)

# Parse dates
data_db <- data_db %>%
  mutate(date = dmy(date))

z_score = as.data.frame(scale(data_db$value))
plot(data_db$date,data_db$z_score)
abline(h=0,col=1, lty=1, lwd=1)
abline(h=1,col=4, lty=1, lwd=2)
abline(h=2,col=1, lty=1, lwd=2)
abline(h=3,col=3, lty=1, lwd=2)
abline(h=-1,col=4, lty=1, lwd=2)
abline(h=-2,col=1, lty=1, lwd=2)
abline(h=-3,col=3, lty=1, lwd=2)

# Transform
# To-do: check grouping variables
data_db <- data_db %>%
  # group_by(date,variable) %>%
  mutate(z_score = scale(value),
         z_score = as.numeric(z_score), 
         z_score_fail = ifelse(abs(z_score) >= 3, FALSE, TRUE))

  
# Find failures
data_db_failures <- data_db %>%
  filter(!z_score_fail)


# Check a failing site
data_db %>%
  filter(variable == "v25", 
         site == "leam") %>%  
  openair::timePlot("value")


# Reshape to messy data
data_db_wide <- data_db %>%
  mutate(key = paste(site, variable, sep = "_"))%>%
  select(date, key, value) %>%
  spread(key, value)


# Reshape to messy data
# data_db_wide <- data_db %>%
#  mutate(date = dmy(date), 
 #        key = paste(site, variable, sep = "_")) %>%
#  select(date, key, value) %>%
 # spread(key, value)
