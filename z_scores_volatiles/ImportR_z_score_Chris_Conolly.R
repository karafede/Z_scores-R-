library(dplyr)
library(tidyr)
library(lubridate)
library(devtools)
library(openair)
library(importr)
library(threadr)

# do not run.....this is stuff in UNIX on the gis-dev Linux server
# crontab -e
# Will open vim
# */5 * * * * Rscript /home/federicok/cron_testing/code_prova_sync.R  # 5 every 5 minutes
# */10080 * * * * Rscript /home/federicok/cron_testing/ImportR_z_score_Pedro.R
# remove week before at 10:20 am
# 20 15 * * * Rscript /home/federicok/cron_testing/remove_week_before.R

# run the ImportR script to generate the .csv file with failures
# 21 15 * * * Rscript /home/federicok/cron_testing/ImportR_z_score_Pedro.R

# email .csv file in the folder to recipient
# 22 15 * * * mail -s "Volatile failures of the week (waq z_scores)" -a /home/federicok/cron_testing/*.csv federico.karagulian@ricardo.com

# 22 15 * * * mail -s "Volatile failures of the week (waq z_scores)" -a /home/federicok/cron_testing/*.csv airqualitymonitoring@ricardo.com


# run importR script and send an email at 10:21 am  containing a .csv file with failures
# 6 11 * * * Rscript /home/federicok/cron_testing/ImportR_z_score_Pedro.R | mail -s "prova" -a /home/federicok/cron_testing/*.csv federico.karagulian@ricardo.com


# */1 * * * * Rscript /home/federicok/cron_testing/code_prova_sync.R


# Set a working directory as in gisdev server
# setwd("/home/federicok/cron_testing")
setwd("C:/z_scores_volatiles")

info_sites <- search_database("waq", "v10|v25")    ### metadata
info_sites <- subset(info_sites, variable %in% c("v10", "v25")) ### metadata

# Make a site vector to use in importr functions
site_vector <- unique(info_sites$site)

# setup start_date that is one week before the current date
week_before <- Sys.time()-604800  # 60 seconds * 60 minutes * 24 hours * 7 days = 1 week 
week_before <- as.character(week_before)
week_before <- str_sub(week_before, start = 1, end = -10) # read only 10 characters (the date)

stats_volatile <- import_stats("waq",site = site_vector, 
                                    variable = c("v10", "v25"),
                                    # start = "2016-01-01",
                                    start = week_before,
                                    # end = "2016-03-31",
                                    statistic = "daily_mean",
                                    extra = TRUE)


# data_volatile <- import_measures("waq", site = site_vector,
#                                      variable = c("v10", "v25"))

# Transform
# To-do: check grouping variables
stats_volatile <- stats_volatile %>%
  group_by(date) %>%      #### only date
  # group_by(date, variable) %>%
  mutate(z_score = scale(value),
         z_score = as.numeric(z_score), 
         z_score_fail = ifelse(abs(z_score) >= 3, FALSE, TRUE))

# Find failures
data_db_failures <- stats_volatile %>%
  filter(!z_score_fail)

tm <- Sys.time() # current date and time
# tm <- Sys.Date()
DATE <- as.character(tm)
# Remove odd characters
DATE <- gsub(":| |-", "_", DATE)

DATE <- str_sub(DATE, start = 1, end = -10) # read only 10 characters (the date)

write.csv(data_db_failures, file = paste0("z_scores_",DATE, "_FAILS.csv"), row.names=FALSE)



