# This script holds three "helper functions" called by the compile_XXX_data functions.

# extract_deployment_dates()
# convert_timestamp_to_datetime()
# add_metadata()


# extract_deployment_dates() ----------------------------------------------

# function to convert deployment and retrieval dates to datetimes
# option to trim data to these dates in the compile_xx_data() functions

# deployment.dates is a dataframe with two columns: start.date and end.date
# there should be one observation in each column
# each observation must be a Date object

# returns a dataframe with 1 observation in two columns: start_date and end_date
# start_date holds a datetime for the start of the deployment (with time of 00:00:00)
# end_date holds a datetime for the end of the deployment (with time of 23:59:59)


#'@importFrom tidyr separate
#'@importFrom lubridate as_datetime

extract_deployment_dates <- function(deployment.dates){

  # name deployment.dates
  names(deployment.dates) <- c("start.date", "end.date")

  # paste date and time and convert to a datetime object
  start_date <- as_datetime(paste(deployment.dates$start.date, "00:00:00"))
  end_date <- as_datetime(paste(deployment.dates$end.date, "23:59:59"))

  # return start and end datetimes
  data.frame(start = start_date, end = end_date)
}


# convert_timestamp_to_datetime() -----------------------------------------

#' @importFrom janitor convert_to_datetime
#' @importFrom lubridate parse_date_time

# function to convert the timestamp to a POSIXct object
# if the date can be converted to class numeric, then it was stored as a number in Excel
## and we have to use janitor::convert_to_datetime to convert to POSIXct.
# Otherwise the date should be a character string that can be converted to POSIXct using
## lubridate::parse_date_time()

convert_timestamp_to_datetime <- function(sensor.data){

  date_format <- sensor.data$TIMESTAMP[1]  # first datetime value; use to check the format

  # if saved as a number in Excel
  if(!is.na(suppressWarnings(as.numeric(date_format)))) {

    sensor.data <- sensor.data %>%
      mutate(TIMESTAMP = janitor::convert_to_datetime(as.numeric(TIMESTAMP)))

  } else{

    # if saved as a character string in Excel
    parse.orders <- c("ymd IMS p", "Ymd IMS p",
                      "Ymd HM", "Ymd HMS",
                      "dmY HM", "dmY HMS",
                      "dmY IM p", "dmY IMS p")

    if(!is.na(suppressWarnings(parse_date_time(date_format, orders = parse.orders)))){

      sensor.data <- sensor.data %>%
        mutate(TIMESTAMP = lubridate::parse_date_time(TIMESTAMP, orders = parse.orders))

    } else {

      # Error message if the date format is incorrect
      stop("Timestamp is not in a format recognized by the strings package.
           See help files for more information.")
    }
  }

  sensor.data
}


# add_metadata() ----------------------------------------------------------

#'@importFrom dplyr add_row

# function to add metadata rows to HOBO, aquaMeasure, and Vemco data
# called by compiled_hobo_data(), compile_aquaMeasure_data(), and compile_vemco_data()

# data.char has three columns of class character: INDEX, TIMESTAMP, PLACEHOLDER
# row1, row2, row3, and row4 are metadata to add to the TIMESTAMP and PLACEHOLDER columns
# row4[1] is added to the TIMESTAMP column; row4[2] is added to the PLACEHOLDER column

# data.char is returned with the additional rows

add_metadata <- function(data.char, row1, row2, row3, row4){

  data.char %>%
    add_row(INDEX = as.character(-1), TIMESTAMP = row4[1], PLACEHOLDER = row4[2], .before = 1) %>%
    add_row(INDEX = as.character(-2), TIMESTAMP = row3, PLACEHOLDER = row3, .before = 1) %>%
    add_row(INDEX = as.character(-3), TIMESTAMP = row2, PLACEHOLDER = row2, .before = 1) %>%
    add_row(INDEX = as.character(-4), TIMESTAMP = row1, PLACEHOLDER = row1, .before = 1)
}

