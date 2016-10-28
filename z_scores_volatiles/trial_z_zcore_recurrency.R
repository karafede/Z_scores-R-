library(dplyr)
library(tidyr)
library(lubridate)

stop.date.time.1 <- as.POSIXct("2016-01-16 12:00:00 EST") # time of last afternoon execution. 
stop.date.time.2 <- as.POSIXct("2016-01-16 16:45:00 EST") # time of last morning execution.
NOW <- Sys.time() 

lapse.time <- 24 * 60 * 60              # A day's worth of time in Seconds
all.exec.times.1 <- seq(stop.date.time.1, NOW, -lapse.time) # all of afternoon execution times. 
all.exec.times.2 <- seq(stop.date.time.2, NOW, -lapse.time) # all of morning execution times. 
all.exec.times <- sort(c(all.exec.times.1, all.exec.times.2)) # combine all times and sort from recent to future
cat("To execute your code at the following times:\n"); print(all.exec.times)



for (i in seq(length(all.exec.times))) {   # for each target time in the sequence
  ## How long do I have to wait for the next execution from Now
  wait.time <- difftime(Sys.time(), all.exec.times[i], units="secs") # calc difference in seconds.
  cat("Waiting for", wait.time, "seconds before next execution\n")
  if (wait.time > 0) {
    Sys.sleep(wait.time)   # Wait from Now until the target time arrives (for "wait.time" seconds)
    ## Put your execution code or function call here
    
    # Load data
    data_db <- read.csv("C:/RICARDO-AEA/z_scores_volatiles/v10_and_v25_daily_means_for_92_sites_2015-07-01_to_2015-09-30.csv")
    head(data_db)
    
    # Parse dates
    data_db <- data_db %>%
      mutate(date = dmy(date))
    
    # Transform
    # To-do: check grouping variables
    data_db <- data_db %>%
      group_by(date) %>%      #### only date
      # group_by(date, variable) %>%
      mutate(z_score = scale(value),
             z_score = as.numeric(z_score), 
             z_score_fail = ifelse(abs(z_score) >= 3, FALSE, TRUE))
    
    
    # Find failures
    data_db_failures <- data_db %>%
      filter(!z_score_fail)
    
    write.csv(data_db_failures, file = "C:/RICARDO-AEA/z_scores_volatiles/data_db_FAILURES.csv", row.names=FALSE)
    
    }
  }
