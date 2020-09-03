## code to prepare `wide_data` dataset goes here

# libraries
library(dplyr)   # for piping and data manipulation functions
library(strings) # to compile data

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


# SECTION 3: Compile data -------------------------------------------------

hobo_data <- compile_HOBO_data(path = path,
                             deployment.range = deployment,
                             area.name = area,
                             trim = trim_dates,
                             # hobo
                             serial.table.HOBO = serial.table.HOBO)


usethis::use_data(hobo_data, overwrite = TRUE)




