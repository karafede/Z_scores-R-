

library(threadr)

setwd("/home/federicok/cron_testing_time")

filenames_txt <- list.files(path = "/home/federicok/cron_testing_time", pattern = "txt")
file.remove(filenames_txt)
