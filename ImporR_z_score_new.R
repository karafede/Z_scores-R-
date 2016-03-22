library(dplyr)
library(tidyr)
library(lubridate)
library (devtools)

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


stats_volatile <- import_statistics("archive",site = site_vector, 
                                    variable = c("v10", "v25", "no2"),
                                    start = "2015-10-01", 
                                    end = "2015-12-31",
                                    statistic = "daily_mean",
                                    extra = TRUE)


data_volatile <- import_measurements("archive", site = site_vector,
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





