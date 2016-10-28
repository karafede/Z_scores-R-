

# source("C:/RICARDO-AEA/z_scores_volatiles/code_prova_sync.r")


while(TRUE) {
  run.at <- as.POSIXct('2016-01-17 18:34')
  if ( trunc(Sys.time(),'min') == run.at)
    source("C:/RICARDO-AEA/z_scores_volatiles/code_prova_sync.r")
  Sys.sleep(10)
}
