

library(openair)
library(importr)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

setwd("h:/volatiles")

info_sites <- search_database("archive", "v10|v25")    ### metadata on volatile PM10 and PM2.5
info_sites <- subset(info_sites, variable %in% c("v10", "v25")) ### metadata

# Make a site vector to use in importr functions
site_vector <- unique(info_sites$site)

############################################################################################


sd_volatile <- import_measures("archive", site_vector,
                               start = "2016-01-01", end = "2016-09-30", 
                               variable = c("v10", "V25"))
# sd_volatile <- sd_volatile[!is.na(sd_volatile$v10),] 
# sd_volatile <- sd_volatile[!is.na(sd_volatile$v25),] 

# check structure of yout data
str(sd_volatile)

# calculate standard deviation by day (average every 24h)
sd_volatile <- sd_volatile %>%
  mutate(date = ymd_hms(date, tz = "UTC"),
         date = str_sub(date, start = 1, end = -10),  # read only 10 characters (the date),
         hour = hour(date)) %>%
  group_by(date,
           site) %>%
  summarise(sd_v10 = sd(v10, na.rm = TRUE),
            sd_v25 = sd(v25, na.rm = TRUE)) %>%
  ungroup()

# check structure of yout data
str(sd_volatile)

# re-put right data-format
sd_volatile$date <- as.Date(sd_volatile$date)
str(sd_volatile)

# make plot in openair for each site
openair::timePlot(sd_volatile, "sd_v10", type = "site")


# search for one specific site
sd_volatile_by_site <- sd_volatile %>%
  filter(site == "bple") %>%
  openair::timePlot("sd_v10", type = "site") 


#############################################################
#One site only
#sd_barnstaple <- import_measures("archive", site="bple",
                                 start = "2015-01-01", end = "2016-09-30", 
                                 variable ="v10")

# check structure of yout data
#str(sd_barnstaple)

# calculate standard deviation by day (average every 24h)
#sd_barnstaple <- sd_barnstaple %>%
#  mutate(date = ymd_hms(date, tz = "UTC"),
#         date = str_sub(date, start = 1, end = -10),  # read only 10 characters (the date),
#         hour = hour(date)) %>%
#  group_by(date,
#           site) %>%
#  summarise(sd_v10 = sd(v10, na.rm = TRUE)) %>%
 # ungroup()

# check structure of yout data
#str(sd_barnstaple)

# re-put right data-format
#sd_barnstaple$date <- as.Date(sd_barnstaple$date)
#str(sd_barnstaple)

# make plot in openair for each site
#  openair::timePlot(sd_barnstaple, "sd_v10", type = "site")
###########################################################
# Reshape to messy data - transpose data
sd_v10_volatile_wide <- sd_volatile %>%
  mutate(key = paste(site, sep = "_"))%>%
  select(date, key, sd_v10) %>%
  spread(key, sd_v10) 

sd_v10_volatile_wide[is.na(sd_v10_volatile_wide)] <- " "
#sd_v10_volatile_wide <- as.Date(sd_v10_volatile_wide$date)
write.csv(sd_v10_volatile_wide, "sd_v10_volatile_wide.csv")

#openair::summaryPlot(sd_v10_volatile_wide) # only works without activating the command line 94 and activating 95

sd_v25_volatile_wide <- sd_volatile %>%
  mutate(key = paste(site, sep = "_"))%>%
  select(date, key, sd_v25) %>%
  spread(key, sd_v25) 

sd_v25_volatile_wide[is.na(sd_v25_volatile_wide)] <- " "

write.csv(sd_v25_volatile_wide, "sd_v25_volatile_wide.csv")

# data_volatile <- data_volatile %>%
#   group_by(site) %>%
#   mutate(stdev = sd((v10))) 

# data_volatile <- import_measures("archive", site = "abd",
#                                  start = "2016-07-01", end = "2016-09-30",
#                                  variable = c("v10"))
# diff <- (data_volatile$v10) - 2.745
# diff <- diff^2
# diff_mean <- mean(diff)
# diff_sqrt <- sqrt(diff_mean)

############################################################################
#Sviluppando in R.....

data_info <- search_database("archive", extra = TRUE)

aun_data <- data_info %>% 
  filter(network_id %in% c("aun"),
         variable_friendly == "v10") 

site_network_vector <- unique(aun_data$site_network_id)

v10_stats<- import_stats ("archive", site_network_id = site_network_vector, 
                              statistic="daily_mean", start=2016)

V10_import <- import_measures("archive", site_network_id = site_network_vector,
                                  variable = "v10",extra = TRUE)

V10_merged <- merge(v10_stats, aun_data,all=T, by ="site_network_id") 

V10_merged <-
  V10_merged %>%
  select(date, value,site.x, site_type)

colnames(V10_merged)<-c("date", "daily_mean", "site", "site_type") 

V10_final <- merge(V10_merged, sd_volatile,all=T, by = c("date","site"))

#Add missing site types

write.csv(V10_final, file = "v10_final.csv", row.names=FALSE)
V10_final <- read.csv("v10_final.csv",header=TRUE)


V10_final <- V10_final %>%
  mutate(date = dmy(date, tz = "UTC"))


search_database ("archive", "sotr")
#ggplot charts - by site type

#ggplot(V10_final, aes(date, sd_v10, colour= site)) + 
# geom_line() +
# theme(legend.position="none") + 
#facet_wrap("site_type") + ylim(0, 5)


ggplot(V10_final, aes(date, sd_v10)) + 
  theme_bw() +
  geom_line() + geom_smooth()+
  theme(legend.position="none") + 
  facet_wrap("site_type") + ylim(0, 5)  + 
  geom_hline(yintercept=3, col="red") +
  geom_hline(yintercept=0.5, col="red")




