## code to prepare `wide_data` dataset goes here

# libraries
library(dplyr)   # for piping and data manipulation functions
library(readr)   # to write csv file
library(strings) # to compile data
library(lubridate)

# SECTION 1: Define the path and variables ------------------------------------------------------------------

# path to Log, Hobo, aquaMeasure, and Vemco folders
path <- file.path("data-raw")

# trim the observations to the deployment and retrieval dates?
trim_dates <- TRUE

# SECTION 2: Extract deployment information from the log ------------------
# Only modify this section if one type of sensor is not included on the string.
# Set the argument for this sensor to NULL

# extract info from the deployment log
log_info <- read_deployment_log(path)

# station name
area = log_info$area.info$station

# deployment dates
deployment <- log_info$deployment.dates

# hobo serial table
serial.table.HOBO <- log_info$HOBO

# aquaMeasure serial table
serial.table.aM <- log_info$aM

# Vemco
depth.vemco <- log_info$vemco$DEPTH


# SECTION 3: Compile data -------------------------------------------------

ALL_data <- compile_all_data(path = path,
                             deployment.range = deployment,
                             area.name = area,
                             trim = trim_dates,
                             # hobo
                             serial.table.HOBO = serial.table.HOBO,
                             # aquaMeasure
                             serial.table.aM = serial.table.aM,
                             # vemco
                             depth.vemco = depth.vemco)

wide_data <- ALL_data


# Thin out so data file is smaller ----------------------------------------

metadata <- data.frame(wide_data[1:4, ])

# HOBO data - measured every hour
wide_data1 <- ALL_data[-c(1:4), 1:2] %>% na.omit() %>%
  mutate(INDEX = c(1:n()))

# aquaMeasure data - measured every 10 minutes
wide_data2 <- ALL_data[-c(1:4), 3:6] %>% na.omit() %>%
  filter(row_number() %% 6 == 0) %>%
  mutate(INDEX = c(1:n()))

# vemco data - measured every minute until 2019-06-13
wide_data3 <- ALL_data[, 7:8]
wide_data_fix <- wide_data3[-c(1:4), ] %>%
  rename(TIMESTAMP = 1, VALUE = 2) %>%
  mutate(TIMESTAMP = as_datetime(TIMESTAMP)) %>%
  mutate(FILTER = if_else(TIMESTAMP < as_datetime("2019-06-13 18:00:00"), 60, 1)) %>%
  filter(row_number() %% FILTER == 0) %>%
  mutate(TIMESTAMP.y.y = as.character(TIMESTAMP), PLACEHOLDER.y.y = as.character(VALUE)) %>%
  select(-TIMESTAMP, -VALUE, -FILTER) %>%
  mutate(INDEX = c(1:n()))

# join together
wide_all <- full_join(wide_data1, wide_data2, by = "INDEX") %>%
  full_join(wide_data_fix, by = "INDEX") %>%
  select(-INDEX)

wide_data <- rbind(metadata, wide_all)

#x <- convert_to_tidydata(wide_data)

#plot_variables_at_depth(x, vars.to.plot = c("Temperature", "Dissolved Oxygen"))



usethis::use_data(wide_data, overwrite = TRUE)



