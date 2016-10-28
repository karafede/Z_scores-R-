

while(TRUE) {

  # start.date.time <- as.POSIXct("2016-01-17 17:55:00 EST") # start time 
  NOW <- Sys.time()
  stop.date.time <- as.POSIXct("2016-01-18 15:10:00 EST") # stop time
  
  lapse.time.stop <- 5*60         # run  code every 60 secs
  all.exec.times.stop <- seq(stop.date.time, NOW, -lapse.time.stop)
  all.exec.times <- sort(all.exec.times.stop)
  cat("To execute your code at the following times:\n"); print(all.exec.times)
  
 for (i in seq(length(all.exec.times))) {   
    wait.time <- difftime(NOW, all.exec.times[i], units="secs") # calc difference in seconds.

     if (wait.time > 0) {
     # Sys.sleep(wait.time)
     # Sys.sleep(300)  ### 5 min
        Sys.sleep(300)  ### 60 secs
    source("C:/z_scores_volatiles/code_prova_sync.r")
       
  }
   }
}
