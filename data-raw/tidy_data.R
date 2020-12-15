## code to prepare `tidydata` dataset goes here

# libraries
library(dplyr)   # to pipe and manipulate data
library(readr)   # to read and write csv files
library(strings) # for string data functions
library(viridis) # because 7 depths

# Raw data ----------------------------------------------------------------

# import raw data
load("data/wide_data.rda")

dat_raw <- convert_to_tidydata(wide_data)

#  Trim data --------------------------------------------------------------

# info  to trim the temperature data
sensors.temp <- c("HOBO", "aquaMeasure", "VR2AR")
start.temp <- "2019-05-30  7:34:00 PM"
end.temp <- "2019-10-19  2:01:00 PM"

# info to trim the DO data
sensors.DO <- c("aquaMeasure")
start.DO <- "2019-05-30 7:34:00 PM"
end.DO <- "2019-06-28 00:00:00 PM"


dat_trim <- dat_raw %>%
  # trim temperature data
  trim_data(var.to.trim = "Temperature",
            start.datetime = start.temp,
            end.datetime = end.temp,
            sensors.to.trim = sensors.temp) %>%
  # trim DO data
  trim_data(var.to.trim = "Dissolved Oxygen",
            start.datetime = start.DO,
            end.datetime = end.DO,
            sensors.to.trim = sensors.DO)

# # keep every 5th observation to reduce size of file
# tidy_data <- dat_trim %>%
#   filter(row_number() %% 5 == 0)


#tidy_data <- dat_trim

# plot_variables_at_depth(tidy_data,
#                         vars.to.plot = c("Temperature", "Dissolved Oxygen"))


 usethis::use_data(tidy_data, overwrite = TRUE)


