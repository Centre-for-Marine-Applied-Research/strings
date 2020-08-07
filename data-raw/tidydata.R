## code to prepare `tidydata` dataset goes here

# libraries
library(dplyr)   # to pipe and manipulate data
library(readr)   # to read and write csv files
library(strings) # for string data functions
library(viridis) # because 7 depths


# path to the raw data file
path <- file.path("C:/Users/Danielle Dempsey/Desktop/RProjects/strings_working/strings_example_data/Birchy Head 2019-05-02")


# Raw data ----------------------------------------------------------------

# file name
file.name <- "Birchy Head_2019-05-02_TEMP_DO_SAL"

# import raw data
dat_raw <- read_csv(paste(path, "/", file.name, "_raw.csv", sep = ""),
                    col_names = FALSE)

dat_raw <- convert_to_tidydata(dat_raw)

# plot raw data
plot_variables_at_depth(dat_raw,
                        vars.to.plot = c("Temperature", "Dissolved Oxygen", "Salinity"),
                        color.palette = pal)


#  Trim data --------------------------------------------------------------

# info  to trim the temperature data
sensors.temp <- c("HOBO", "aquaMeasure", "VR2AR")
start.temp <- "2019-05-02  7:48:51 PM"
end.temp <- "2019-11-22  2:30:22 PM"

# info to trim the DO data
sensors.DO <- c("aquaMeasure")
start.DO <- "2019-05-02  7:48:51 PM"
end.DO <- "2019-11-22  2:30:22 PM"

# info to trim the salinity data
sensors.sal <- c("aquaMeasure")
start.sal <- "2019-05-02  7:48:51 PM"
end.sal <- "2019-11-22  2:30:22 PM"


# trim temperature data
dat_trim <- trim_data(dat_raw,
                      var.to.trim = "Temperature",
                      start.datetime = start.temp,
                      end.datetime = end.temp,
                      sensors.to.trim = sensors.temp)

# trim DO data **pass the dataframe with trimmed temperature data**
dat_trim <- trim_data(dat_trim,
                      var.to.trim = "Dissolved Oxygen",
                      start.datetime = start.DO,
                      end.datetime = end.DO,
                      sensors.to.trim = sensors.DO)

# trim salinity data **pass the dataframe with trimmed temperature & DO data**
dat_trim <- trim_data(dat_trim,
                      var.to.trim = "Salinity",
                      start.datetime = start.DO,
                      end.datetime = end.DO,
                      sensors.to.trim = sensors.DO)

# remove 15 m for example purposes and only keep every 5th obs to reduce size;
# could also reduce time
dat_trim <- dat_trim %>%  filter(DEPTH != "15") %>%
  mutate(DEPTH = ordered(DEPTH)) %>%
  filter(row_number() %% 5 == 1)

# plot trimmed data
plot_variables_at_depth(dat_trim,
                        vars.to.plot = c("Temperature", "Dissolved Oxygen", "Salinity"))

tidydata <- dat_trim

usethis::use_data(tidydata, overwrite = TRUE)


