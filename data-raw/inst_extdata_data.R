# January 15, 2021

# read in raw data, filter to make the file size smaller, and export to inst/extdata

library(dplyr)
library(readr)
library(lubridate)
library(stringr)


# aquaMeasure -------------------------------------------------------------


aM <- read_csv("data-raw/aquaMeasure/aquaMeasure-670364_2019-10-19_140933_UTC.csv")

rm_aM <- c("Power Up", "Text", "Device Tilt", "Battery Voltage", "Time Set")

aM_out <- aM %>%
  filter(!(`Record Type` %in% rm_aM)) %>%
  arrange(`Record Type`) %>%
  filter(row_number() %% 20 == 0)

(nrow(aM) - nrow(aM_out))/nrow(aM)

write_csv(aM_out, file = "inst/extdata/aquaMeasure/aquaMeasure-670364_2019-10-19_UTC.csv")

# Vemco -------------------------------------------------------------------

vemco <- read_csv("data-raw/Vemco/Borgles_Island_2019_05_30.csv",
                  col_names = TRUE,
                  col_types = cols(.default = col_character()))

vemco_out <- vemco %>%
  filter(Description == "Temperature",
         row_number() %% 10 == 0)

write_csv(vemco_out, file = "inst/extdata/vemco/Vemco_Borgles_Island_2019_05_30.csv")

# library(strings)
# x <- compile_vemco_data(path = "C:/Users/Danielle Dempsey/Desktop/RProjects/strings/inst/extdata",
#                         depth.vemco = depth,
#                         deployment.range = deployment)
#
# x_tidy <- convert_to_tidydata(x[,-1])
#
# plot_variables_at_depth(x_tidy, vars.to.plot = c("Temperature"))


# HOBO --------------------------------------------------------------------


# This doesn't work because the deg C symbol gets corrupted and breaks the compile_HOBO_data function

# hobo <- read_csv("data-raw/Hobo/10755220.csv",
#                   col_names = FALSE,
#                   col_types = cols(.default = col_character()))
#
# hobo_colnames <- hobo[1,]
#
# hobo_out <- hobo %>%
#   slice(-1) %>%
#   filter(row_number() %% 4 == 0)
#
# hobo_out <- rbind(hobo_colnames, hobo_out)
#
# write_csv(hobo_out, file = "inst/extdata/Hobo/10755220.csv", col_names = FALSE)
#
# y <- compile_HOBO_data(path = "C:/Users/Danielle Dempsey/Desktop/RProjects/strings/inst/extdata",
#                        serial.table.HOBO = data.frame("SENSOR" = "HOBO-10755220", "DEPTH" = "2m"),
#                        deployment.range = deployment)
#
# y_tidy <- convert_to_tidydata(y[,-1])
#
# plot_variables_at_depth(y_tidy, vars.to.plot = c("Temperature"))
#

