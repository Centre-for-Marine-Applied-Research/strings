#'@title Trim data to specified start and end datetimes
#'@param dat.tidy Data in a tidy format, as exported by the
#'  \code{convert_to_tidydata()} function.
#'@param var.to.trim Variable to trim, e.g. "Temperature", "Dissolved Oxygen",
#'  or "Salinity".
#'@param start.datetime A character string indicating the date and time that the
#'  sensor started reliably recording data, in one of the following orders: "ymd
#'  IMS p", "Ymd IMS p", "Ymd HM", "Ymd HMS".
#'@param end.datetime A character string indicating the date and time the sensor
#'  stopped reliably recording data, in one of the following orders: "ymd IMS
#'  p", "Ymd IMS p", "Ymd HM", "Ymd HMS".
#'@param sensors.to.trim Sensor type(s) for which to trim the data. Default is
#'  \code{sensors.to.trim = c("HOBO", "aquaMeasure", "VR2AR")}.
#'@return Returns tidy.data trimmed to start.datetime to end.datetime
#'@family format
#'@author Danielle Dempsey
#'@importFrom lubridate parse_date_time
#'@importFrom tidyr separate
#'@import dplyr
#'@export


trim_data <- function(dat.tidy,
                      var.to.trim,
                      start.datetime = min(dat.tidy$DATE),
                      end.datetime = max(dat.tidy$DATE),
                      sensors.to.trim = c("HOBO", "aquaMeasure", "VR2AR")){

  # convert start/end datetimes to POSIXct
  parse.orders <- c("ymd IMS p", "Ymd IMS p", "Ymd HM", "Ymd HMS")

  start.datetime <- parse_date_time(start.datetime, orders = parse.orders)
  end.datetime <- parse_date_time(end.datetime, orders = parse.orders)

  # remove observations of var.to.trim
  dat <- dat.tidy %>% filter(VARIABLE != var.to.trim)

  # filter dat.tidy for var.to.trim and make column of SENSOR_TYPE
  dat_var <- dat.tidy %>%
    filter(VARIABLE == var.to.trim) %>%
    separate(col = SENSOR, into = c("SENSOR_TYPE", NA),
             sep = "-| - ", remove = FALSE)

  # Error message in case trying trim a sensor that is not in the dataframe OR
  # a sensor is spelled wrong
  if(any(!(sensors.to.trim %in% unique(dat_var$SENSOR_TYPE)))){

    stop(paste("You are trying to trim data from a sensor that did not measure ", var.to.trim,
         ". Check spelling in sensors.to.trim", sep = ""))
  }

  # data for var.to.trim measured by sensors we are NOT interested in
  dat_var_other <- dat_var %>%
    filter(!(SENSOR_TYPE %in% sensors.to.trim)) %>%
    select(-SENSOR_TYPE)

  # filter for sensor type(s) and date range of interest
  dat_var_trim <- dat_var %>%
    filter(SENSOR_TYPE %in% sensors.to.trim,
           DATE >= start.datetime,
           DATE <= end.datetime) %>%
    select(-SENSOR_TYPE)

  # bind and return data
  dat_out <- rbind(dat, dat_var_other, dat_var_trim) %>%
    arrange(VARIABLE, DEPTH)

  dat_out

}
