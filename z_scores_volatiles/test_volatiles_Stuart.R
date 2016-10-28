# Set-up
suppressMessages(library(threadr))
library(importr)

# Set working directory
setwd("/home/stuartg/volatile_standardisation")

# Get date ranges
date_system <- ymd(Sys.Date())
date_start <- floor_date(date_system, "quarter")
date_end <- ceiling_date(date_system, "quarter")

# Convert to characters for function use
date_start <- as.character(date_start)
date_end <- as.character(date_end)

# Query database for sites which have the variables
data_sites <- search_database("archive") %>% 
  filter(variable %in% c("v25", "v10"))

# Get site network ids
vector_ids <- data_sites$site_network_id

# Query database for daily means
data_daily <- import_stats(
  "archive", site_network_id = vector_ids, start = date_start, end = date_end, 
  statistic = "daily_mean")  

# Select useful variables
data_daily <- data_daily %>% 
  select(date,
         site,
         site_name, 
         variable,
         site_network_id,
         value)

# Standadise variables
# Brian's document has only used date as a group, not site or variable
data_daily <- data_daily %>% 
  group_by(date) %>% 
  mutate(standardised_value = scale(value),
         standardised_value = as.numeric(standardised_value), # bug?
         standardised_value_pass = ifelse(abs(standardised_value) <= 3, 
                                          TRUE, FALSE)) %>% 
  ungroup() %>% 
  mutate(date = ymd(date))

# Filter to failed observations
data_daily_fail <- data_daily %>% 
  filter(!standardised_value_pass) %>% 
  arrange(variable,
          site,
          date) %>% 
  arrange(date)

# Export files
# File name
file_name <- paste0("data/", date_start, "_volatile_standardisation_failures")

# Export table
write.csv(data_daily_fail, str_c(file_name, ".csv"), row.names = FALSE)

# Export json
json_string <- jsonlite::toJSON(data_daily_fail, pretty = TRUE)
write(json_string, str_c(file_name, ".json"))

# Print json
message(json_string)

# rsync call
system("./rsync_volatile_standardisation_failures_to_ash.sh")
