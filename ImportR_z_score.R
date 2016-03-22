library(dplyr)
library(tidyr)
library(lubridate)
#library (devtools)
#library(mail)
#library(sendmailR)
#library(mailR)

# Load data
data_db <- read.csv("C:/z_scores_volatiles/v10_and_v25_daily_means_for_92_sites_2015-07-01_to_2015-09-30.csv")
head(data_db)

# Parse dates
data_db <- data_db %>%
  mutate(date = dmy(date))

# data_db <- group_by(data_db,date,variable)
# data_db$z_score = (scale(data_db$value))
# plot(data_db$date,data_db$z_score)
# abline(h=0,col=1, lty=1, lwd=1)
# abline(h=1,col=4, lty=1, lwd=2)
# abline(h=2,col=1, lty=1, lwd=2)
# abline(h=3,col=3, lty=1, lwd=2)
# abline(h=-1,col=4, lty=1, lwd=2)
# abline(h=-2,col=1, lty=1, lwd=2)
# abline(h=-3,col=3, lty=1, lwd=2)

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

target <- c("leam", "abd")
AAA <-  filter(data_db, variable == "v25", site %in% target)



data_db %>%
  filter(variable == "v25",
          site == "leam") %>%  
  openair::timePlot("z_score") 


# Check sites with only PM2.5
data_db %>%
  filter(variable == "v25") %>%  
  openair::timePlot("z_score", type = "site", group = TRUE) 


# Check failing sites with only PM2.5
data_db_failures %>%
  filter(variable == "v25") %>%  
  openair::timePlot("z_score", type = "site") 


# Reshape to messy data
data_db_wide <- data_db %>%
  mutate(key = paste(site, variable, sep = "_"))%>%
  select(date, key, z_score) %>%
  spread(key, z_score)

write.csv(data_db_wide, file = "C:/z_scores_volatiles/data_db_wide.csv", row.names=FALSE)
write.csv(data_db_failures, file = "C:/z_scores_volatiles/data_db_failures.csv", row.names=FALSE)


data_LEAM <- data_db_wide%>%
  select(date, leam_v10, leam_v25)



# # Reshape to messy data
# data_db_wide <- data_db %>%
# mutate(date = dmy(date), 
#           key = paste(site, variable, sep = "_")) %>%
#    select(date, key, value) %>%
#   spread(key, value)


##############################################################################


# Importr stuff
# Instructions
# http://172.31.113.9:3838/importr/importr_outline.html

# Install package from web server
library(devtools)

# Install
# install_url("http://172.31.113.9:3838/importr/package/importr.zip")

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


stats_volatile <- import_statistics("archive", site = site_vector, 
                                    variable = c("v10", "v25"),
                                    start = "2015-07-01", 
                                    end = "2015-09-30",
                                    statistic = "daily_mean",
                                    extra = TRUE)


data_volatile <- import_measurements("archive", site = "bel2", 
                                     variable = c("v10", "v25"))

openair::timePlot(data_volatile, "v25")


data_ox <- import_measurements("archive", site = "ox", 
                               variable = "no2", start = 1970)

openair::timeVariation(data_ox, "no2", group = "season")


#################################################################


info_sites_PM25_arch <- search_database("archive")    ### metadata from "archive"

# unique(info_sites_PM25_arch$variable) ### info on variables
# sort(unique(info_sites_PM25_arch$variable)) ### info on variables

info_sites_PM25_aqeng <- search_database("aqengland", "pm25")    ### metadata from "archive"

info_sites_PM25_arch <- subset(info_sites_PM25_arch, variable %in% c("pm25","gr25")) ### metadata
info_sites_PM25_aqeng <- subset(info_sites_PM25_aqeng, variable %in% "pm25") ### metadata

write.csv(info_sites_PM25_arch, file = "C:/z_scores_volatiles/info_sites_PM25_AURUN_2010_2012.csv", row.names=FALSE)
write.csv(info_sites_PM25_aqeng, file = "C:/z_scores_volatiles/info_sites_PM25_AQENG_2010_2012.csv", row.names=FALSE)


# Make a site vector to use in importr functions
site_vector_arch <- unique(info_sites_PM25_arch$site)
site_vector_aqeng <- unique(info_sites_PM25_aqeng$site)

Annual_PM25_arch <- import_statistics("archive", site = site_vector_arch, 
                                    variable = c("pm25","gr25"),
                                    start = "2010-01-01", 
                                    end = "2012-12-31",
                                    statistic = "annual_mean",
                                    extra = TRUE)

Annual_PM25_aqeng <- import_statistics("aqengland", site = site_vector_aqeng, 
                                     variable = "pm25",
                                     start = "2010-01-01", 
                                     end = "2012-12-31",
                                     statistic = "annual_mean",
                                     extra = TRUE)

write.csv(Annual_PM25_arch, file = "C:/z_scores_volatiles/Annual_Mean_PM25_AURUN_2010_2012.csv", row.names=FALSE)
write.csv(Annual_PM25_aqeng, file = "C:/z_scores_volatiles/Annual_Mean_PM25_AQENG_2010_2012.csv", row.names=FALSE)


############### Send email with R (not working) #####################################

from <- sprintf("<sendmailR@%s>", Sys.info()[4]) 
to <- "<ftir75@gmail.com>" 
subject <- "Hello from R" 
msg <- "my first email" 
sendmail(from, to, subject, msg,control=list(smtpServer="ASPMX.L.GOOGLE.COM")) 


