
library(dplyr)
library(tidyr)
library(lubridate)
library (devtools)
library(leaflet)
library(htmlwidgets)

##############################################################################


# Importr stuff
# Instructions
# http://172.31.113.9:3838/importr/importr_outline.html

# Install package from web server

# Install
#install_url("http://172.31.113.9:3838/importr/package/importr.zip")

# Use it
library(importr)

# Helpers
print_database_names()
print_statistic_types()

# Use look-up table to find site codes

###### ARCHIVE is UK-AIR or AURUN ###################################
#####################################################################

info_sites <- search_database("archive", "v10|v25")    ### metadata
info_sites <- subset(info_sites, variable %in% c("v10", "v25")) ### metadata


# Make a site vector to use in importr functions
site_vector <- unique(info_sites$site)


stats_volatile <- import_stats("archive",site = site_vector, 
                                    variable = c("v10", "v25", "no2"),
                                    start = "2016-06-01", 
                                    end = "2016-06-02",
                                    statistic = "daily_mean",
                                    extra = TRUE)



data_volatile <- import_measures("archive", site = site_vector,
                                     variable = c("v10", "v25", "no2"))



###### correlation plots ################################################

# Basic Scatterplot Matrix
data <- cbind(data_volatile$v10, data_volatile$v25, data_volatile$no2)
colnames(data) <- c("v10", "v25", "no2")
pairs(data,main="Simple Scatterplot Matrix")



?pairs
?lattice::splom
?lattice::panel.xyplot

pairs( data, panel=function(x,y){
  points(x,y)
  abline(lm(y~x), col='red')
})

library(lattice)
splom( ~ data, type=c('p','r') )


# 
# require(gclus)
# png();   judge.cor <- cor(USJudgeRatings)
# judge.color <- dmat.color(judge.cor)
# ?pairs
# #Review the panel functions
# ?cpairs  
# cpairs(USJudgeRatings,panel.colors=judge.color,pch=".",gap=.5, 
#        upper.panel=panel.smooth)
# dev.off()

##############################################################################


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


# Reshape to messy data
data_db_wide <- stats_volatile %>%
  mutate(key = paste(site, variable, sep = "_"))%>%
  select(date, key, z_score) %>%
  spread(key, z_score)


# Check a failing site v2.5
stats_volatile %>%
   filter(variable == "v25") %>%  
  openair::timePlot("z_score", type = "site") 

# Check a failing site v10
stats_volatile %>%
  filter(variable == "v10") %>%  
  openair::timePlot("z_score", type = "site") 



write.csv(stats_volatile, file = "C:/z_scores_volatiles/z_scores_Q4_all.csv", row.names=FALSE)
write.csv(data_db_failures, file = "C:/z_scores_volatiles/z_scores_Q4_FAILS.csv", row.names=FALSE)
write.csv(data_db_wide, file = "C:/z_scores_volatiles/z_scores_Q4_all_wide.csv", row.names=FALSE)


############################################################################################################
############################################################################################################
############################################################################################################
####### Temperatrure data ###################################################################################

setwd("C:/z_scores_volatiles/Temperature_data")

info_sites_ARCHIVE_V10 <- search_database("archive", "v10|v25")    ### metadata
info_sites_ARCHIVE_V10 <- subset(info_sites_ARCHIVE_V10, variable %in% c("v10", "v25")) ### metadata
Archive_v10 <- cbind(info_sites_ARCHIVE_V10$site_name, info_sites_ARCHIVE_V10$latitude, info_sites_ARCHIVE_V10$longitude)
Archive_v10 <- as.data.frame(Archive_v10)
colnames(Archive_v10) <- c("site", "Lat", "Lon")
write.csv(Archive_v10, "Archive_v10.csv")

# make a leaflet map
map <- leaflet(data = Archive_v10[,]) %>% 
  setView(lng = -2, lat = 53.5, zoom = 6) %>%
  addTiles() %>%
  addMarkers(~Lon, ~Lat, popup = ~site)
map


saveWidget(map,
           file="UK_Temp_sites_Ricardo.html",
           selfcontained = FALSE)




#### ///////////////////// ######################################################

info_sites_temp_ARCHIVE <- search_database("archive", "temp", extra = TRUE)    ### metadata
Archive_temp <- as.data.frame(info_sites_temp_ARCHIVE$site_name)
colnames(Archive_temp) <- "site Name"
write.csv(Archive_temp, "Archive_temp.csv")

info_sites_temp_AQENGLAND <- search_database("aqengland", "temp")
AQENGLAND_temp <- as.data.frame(info_sites_temp_AQENGLAND$site_name)
colnames(AQENGLAND_temp) <- "Site name"
write.csv(AQENGLAND_temp, "AQEngland_temp.csv")


info_sites_temp_scotarc <- search_database("scotarc", "temp")
scotarc_temp <- as.data.frame(info_sites_temp_scotarc$site_name)
colnames(scotarc_temp) <- "Site name"
write.csv(scotarc_temp, "scotarc_temp.csv")


info_sites_temp_kent <- search_database("kent", "temp")
kent_temp <- as.data.frame(info_sites_temp_kent$site_name)
colnames(kent_temp) <- "Site name"
write.csv(kent_temp, "kent_temp.csv")

info_sites_temp_nlincs <- search_database("nlincs", "temp")

info_sites_temp_ndarchive <- search_database("ndarchive", "temp")
ndarchive_temp <- as.data.frame(info_sites_temp_ndarchive$site_name)
colnames(ndarchive_temp) <- "Site name"
write.csv(ndarchive_temp, "ndarchive_temp.csv")

info_sites_temp_waq <- search_database("waq", "temp")
waq_temp <- as.data.frame(info_sites_temp_waq$site_name)
colnames(waq_temp) <- "Site name"
write.csv(waq_temp, "waq_temp.csv")






site_vector_temp <- unique(info_sites_temp_ARCHIVE$site)
temperature_data <- import_measures("archive",site = site_vector_temp, 
                                    variable = "temp",
                                    start = "2016-04-01", 
                                    end = "2016-04-10",
                                    extra = TRUE)

temperature_data_Oxford <- import_measures("archive",site = "OX8", 
                                           variable = "temp",
                                           start = "2016-04-01", 
                                           end = "2016-04-10",
                                           extra = TRUE)
