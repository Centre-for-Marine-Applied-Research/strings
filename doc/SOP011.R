## ---- include = FALSE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----set-options, echo=FALSE, cache=FALSE-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
options(width=300)

## ----setup, message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(strings)
library(dplyr)

## ---- message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
wide_data %>% 
  convert_to_tidydata() %>% 
  group_by(VARIABLE) %>% 
  summarise(MIN_TIMESTAMP = min(TIMESTAMP), MAX_TIMESTAMP = max(TIMESTAMP)) 


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# specify the timestamp for the FIRST VALID measurement
# (this can be copy/pasted from the _raw.csv file)
start.timestamp <- "2019-05-30  7:34:00 PM"

# specify the timestamp for the LAST VALID measurement 
# (this can be copy/pasted from the _raw.csv file)
last.timestamp <- "2019-10-19  2:01:00 PM"

# sensors that recorded temperature data that should be trimmed
sensors.temp <- c("HOBO", "aquaMeasure", "VR2AR")

# sensors that recorded dissolved oxygen data that should be trimmed
sensors.DO <- c("aquaMeasure")

# info to trim the salinity data
sensors.sal <- c("aquaMeasure")

# convert the wide data to long data
dat_trim <- wide_data %>% 
  convert_to_tidydata() %>% 
  # trim temperature data
  trim_data(var.to.trim = "Temperature",
            start.datetime = start.timestamp ,
            end.datetime = last.timestamp,
            sensors.to.trim = sensors.temp) %>%
  # trim DO data
  trim_data(var.to.trim = "Dissolved Oxygen",
            start.datetime = start.timestamp ,
            end.datetime = last.timestamp,
            sensors.to.trim = sensors.DO) #%>% 
  # # trim salinity data
  # trim_data(var.to.trim = "Salinity",
  #           start.datetime = start.timestamp ,
  #           end.datetime = last.timestamp,
  #           sensors.to.trim = sensors.sal)


## ---- message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat_trim %>% 
  group_by(VARIABLE) %>% 
  summarise(MIN_TIMESTAMP = min(TIMESTAMP), MAX_TIMESTAMP = max(TIMESTAMP)) 


## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# DATE: 2020-Sep-02
# NAME: DD  
# strings VERSION: 1.1.0
# NOTES:

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Template for trimming data compiled from a sensor string deployment
# Returns trimmed data as a csv file in the final folder on path
# and in the appropriate county folder for transfer to Open Data Portal

# SECTION 1: Import and visualize raw data

# SECTION 2: Trim data

# SECTION 3: Visualize and Export trimmed data

# SECTION 4: Export for Open Data

## ---- message=FALSE---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# libraries
library(dplyr)         # to pipe and manipulate data
library(readr)         # to read and write csv files
library(strings)       # for string data functions
library(ggplot2)       # for DO plot
library(lubridate)     # for DO plot
library(googlesheets4) # for county info

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  
#  # Section 1: Import and visualize raw data ---------------------------------------------------------------
#  
#  # path to the raw data file
#  path <- file.path("Y:/Coastal Monitoring Program/Data_Strings/Birchy Head/Birchy Head 2019-05-02")
#  
#  # path to export the trimmed and formatted data (county name pasted on below)
#  path.export <- file.path("Y:/Coastal Monitoring Program/Open Data/")
#  

## ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Raw data ----------------------------------------------------------------

# file name
file.name <- "Birchy Head_2019-05-02_TEMP_DO"

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # import raw data
#  dat_raw <- read_csv(paste(path, "/", file.name, "_raw.csv", sep = ""),
#                      col_names = FALSE)

## ---- echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat_raw <- wide_data

## ---- fig.width = 8.5, fig.height = 6---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
dat_raw <- convert_to_tidydata(dat_raw)

# plot raw data
plot_variables_at_depth(dat_raw,
                        vars.to.plot = c("Temperature", "Dissolved Oxygen"))


## ---- echo=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
head(wide_data[,1:2], n = 20)


## ---- fig.width = 8.5, fig.height = 6---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# info  to trim the temperature data
sensors.temp <- c("HOBO", "aquaMeasure", "VR2AR")
start.temp <- "2019-05-30  7:34:00 PM"
end.temp <- "2019-10-19  2:01:00 PM"

# info to trim the DO data
sensors.DO <- c("aquaMeasure")
start.DO <- "2019-05-30  7:34:00 PM"
end.DO <- "2019-10-19  2:01:00 PM"

# info to trim the salinity data
# sensors.sal <- c("aquaMeasure")
# start.sal <- "2019-05-02  8:45:22 PM"
# end.sal <- "2019-11-22  2:30:22 PM"

# convert the wide data to long data
dat_trim <- dat_raw %>% 
  # trim tempterature data
  trim_data(var.to.trim = "Temperature",
            start.datetime = start.temp,
            end.datetime = end.temp,
            sensors.to.trim = sensors.temp) %>%
  # trim DO data
  trim_data(var.to.trim = "Dissolved Oxygen",
            start.datetime = start.DO,
            end.datetime = end.DO,
            sensors.to.trim = sensors.DO) #%>% 
  # trim salinity data (you only need this part of the pipe if salinity was measured during the deployment)
  # trim_data(var.to.trim = "Salinity",
  #           start.datetime = start.sal,
  #           end.datetime = end.sal,
  #           sensors.to.trim = sensors.sal)


## ----  fig.width = 8.5, fig.height = 6--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot trimmed data
plot_variables_at_depth(dat_trim,
                        vars.to.plot = c("Temperature", "Dissolved Oxygen"))


## ---- fig.width = 8.5, fig.height = 6---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# info  to trim the temperature data
sensors.temp <- c("HOBO", "aquaMeasure", "VR2AR")
start.temp <- "2019-05-02  8:45:22 PM"
end.temp <- "2019-11-22  2:30:22 PM"

# info to trim the DO data
sensors.DO <- c("aquaMeasure")
start.DO <- "2019-09-02  00:00:00 PM"
end.DO <- "2019-11-22  2:30:22 PM"

# info to trim the salinity data
sensors.sal <- c("aquaMeasure")
start.sal <- "2019-05-02  8:45:22 PM"
end.sal <- "2019-11-22  2:30:22 PM"

# convert the wide data to long data
dat_trim <- dat_raw %>% 
  # trim tempterature data
  trim_data(var.to.trim = "Temperature",
            start.datetime = start.temp,
            end.datetime = end.temp,
            sensors.to.trim = sensors.temp) %>%
  # trim DO data
  trim_data(var.to.trim = "Dissolved Oxygen",
            start.datetime = start.DO,
            end.datetime = end.DO,
            sensors.to.trim = sensors.DO) #%>% 
  # trim salinity data (you only need this part of the pipe if salinity was measured during the deployment)
  # trim_data(var.to.trim = "Salinity",
  #           start.datetime = start.sal,
  #           end.datetime = end.sal,
  #           sensors.to.trim = sensors.sal)


## ---- fig.width = 8.5, fig.height = 6---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# plot trimmed data
plot_variables_at_depth(dat_trim,
                        vars.to.plot = c("Temperature", "Dissolved Oxygen"))


## ---- fig.width = 8.5, fig.height = 3---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# check where DO cutoff is
DO <- plot_variables_at_depth(dat_raw,
                              vars.to.plot = "Dissolved Oxygen")

DO[[1]] + geom_vline(xintercept = as_datetime(start.DO))


## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # export trimmed data
#  write_csv(dat_trim, file = paste(path, "/", file.name, "_trimmed.csv", sep = ""))
#  

## ---- eval=FALSE------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
#  # SECTION 4: Export for Open Data ----------------------------------------------------
#  
#  # read deployment log for the area info
#  log <- read_deployment_log(path)
#  location <- log$area.info
#  
#  # allow access to the google sheet
#  googlesheets4::gs4_deauth()
#  
#  # link to the "STRING TRACKING" google sheet
#  link <- "https://docs.google.com/spreadsheets/d/1a3QvJsvwr4dd64g3jxgewRtfutIpsKjT2yrMEAoxA3I/edit#gid=828367890"
#  
#  # read in the "Area Info" tab of the STRING TRACKING sheet
#  Area_Info <- googlesheets4::read_sheet(link, sheet = "Area Info")
#  
#  # look up the Station name in the Area Info tab and return the county
#  county <- Area_Info[which(Area_Info$Station == location$station), "County"]
#  
#  # warnings if there is more than one entry OR no entries for this station in the Area Info tab
#  if(length(county > 1)) warning(paste("There is more than one station named", location$station, "in the Area Info tab"))
#  if(length(county < 1)) warning(paste("There is no station named", location$station, "in the Area Info tab"))
#  
#  # finish the path.export
#  county <- county$County
#  path.export <- paste(path.export, county, "data", sep = "/")
#  
#  # name for the file
#  open.data.name <- name_for_open_data(file.name)
#  
#  # format for Open Data (add location columns)
#  dat_open <- format_for_opendata(dat_trim, location)
#  
#  # writw to county folder
#  write_csv(dat_open, file = paste(path.export, "/", open.data.name, ".csv", sep = ""))
#  

