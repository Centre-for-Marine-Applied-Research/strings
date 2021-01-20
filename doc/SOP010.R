## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width=300)

## ----setup------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(strings)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data compiled from the three Hobo sensors deployed at Birchy Head on May 2, 2019:
head(hobo_data, n = 10)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

hobo_tidy <- convert_to_tidydata(hobo_data[, -1]) # remove the INDEX column

head(hobo_tidy)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Data compiled from the all sensors deployed at Birchy Head on May 2, 2019:
head(wide_data, n = 10)


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ALL_tidy <- convert_to_tidydata(wide_data) 

head(ALL_tidy)

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# DATE: 2020-Sep-02
# NAME: DD  
# strings VERSION: 1.1.0
# NOTES:

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# SECTION 1: Define the path and variables
# 
# SECTION 2: Extract deployment information from the log
#   Only modify this section if one type of sensor noted in the log is not included on the string.
#   Set the argument for this sensor to NULL
# 
# SECTION 3: Compile data
#   You should not have to modify anything in this section
#   A file will be exported to the path (_raw.csv)
# 
# SECTION 4: Visualize data
#   Import data and visualize

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# libraries
library(dplyr)   # for piping and data manipulation functions
library(readr)   # to write csv file
library(strings) # to compile data

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  ### Section 1: Define the path
#  
#  # path to Log, Hobo, aquaMeasure, and Vemco folders
#  path <- file.path("Y:/Coastal Monitoring Program/Data_Strings/Birchy Head/Birchy Head 2019-05-02")
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # SECTION 2: Extract deployment information from the log ------------------
#  # Only modify this section if one type of sensor noted in the log is not included on the string.
#  # Set the argument for this sensor to NULL
#  
#  # Extract information from the deployment log
#  log_info <- read_deployment_log(path)
#  
#  # Define station name based on the log
#  area = log_info$area.info$station
#  
#  # Define deployment start and end dates based on the log
#  deployment <- log_info$deployment.dates
#  
#  # Create a table of HOBO sensors and deployment depths based on the log
#  serial.table.HOBO <- log_info$HOBO
#  
#  # Create a table of aquaMeasure sensors and deployment depths based on the log
#  serial.table.aM <- log_info$aM
#  
#  # Define the deployment depth of the VR2 sensor based on the log
#  depth.vemco <- log_info$vemco$DEPTH
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  #Compile data from a single deployment
#  ALL_data <- compile_all_data(path = path,
#                               deployment.range = deployment,
#                               area.name = area,
#                               # hobo
#                               serial.table.HOBO = serial.table.HOBO,
#                               # aquaMeasure
#                               serial.table.aM = serial.table.aM,
#                               # vemco
#                               depth.vemco = depth.vemco,)
#  
#  # Name the file
#  file_name <- name_compiled_data(area.name = area,
#                                  deployment.start = deployment$start.date,
#                                  vars = unique(convert_to_tidydata(ALL_data)$VARIABLE))
#  
#  # Write csv file with compiled data
#  write_csv(ALL_data, paste(path, "/", file_name, "_raw.csv", sep = ""), col_names = FALSE)

## ---- echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ALL_raw <- wide_data 
area <- "Borgles Island"

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # Import and plot the raw data
#  ALL_raw <- read_csv(paste(path, "/", file_name, "_raw.csv", sep = ""), col_names = FALSE)
#  

## ---- fig.width = 8.5, fig.height = 6---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ALL_raw_tidy <- convert_to_tidydata(ALL_raw)

# Update vars.to.plot and ylab.units as necessary. 
# Remove any variables and their associated units if they aren't present in your data.
plot_variables_at_depth(ALL_raw_tidy,
                        plot.title = area,
                        vars.to.plot = c("Temperature", "Dissolved Oxygen"))


