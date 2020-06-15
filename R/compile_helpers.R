# This script holds three "helper functions" called by the compile_XXX_data functions.

# extract_deployment_dates()
# convert_timestamp_to_datetime()
# add_metadata()


# extract_deployment_dates() ----------------------------------------------

# function to extract the start date and end date of deployment from the deployment.range argument
# dates are converted to datetimes
# option to trim data to these dates in the compile_xx_data() functions

# deployment.dates is in the format "2018-Nov-15 to 2020-Jan-24"

# returns a dataframe with 1 observation in two columns: start and end
# start holds a datetime for the start of the deployment (with time of 00:00:00)
# end holds a datetime for the end of the deployment (with time of 23:59:59)


#'@importFrom tidyr separate
#'@importFrom lubridate as_datetime

extract_deployment_dates <- function(deployment.dates){

  # extract the deployment start and end dates from deployment.dates
  start_end_date <- separate(data = data.frame(deployment.dates),
                             col = deployment.dates,
                             into = c("start.date", NA, "end.date"), sep  = " " )

  # paste date and time and convert to a datetime object
  start_date <- as_datetime(paste(start_end_date$start.date, "00:00:00"))
  end_date <- as_datetime(paste(start_end_date$end.date, "23:59:59"))

  # return start and end datetimes
  data.frame(start = start_date, end = end_date)
}


# convert_timestamp_to_datetime() -----------------------------------------

#' @importFrom janitor convert_to_datetime
#' @importFrom lubridate parse_date_time

# if the date can be converted to class numeric, then it was stored as a number in Excel
## and we have to use janitor::convert_to_datetime to convert to POSIXct.
# Otherwise the date should be a character string that can be converted to POSIXct using
## lubridate::parse_date_time()

convert_timestamp_to_datetime <- function(sensor.data){

  date_format <- sensor.data$DATE[1]  # first datetime value; use to check the format

  # if saved as a number in Excel
  if(!is.na(suppressWarnings(as.numeric(date_format)))) {

    sensor.data <- sensor.data %>%
      mutate(DATE = janitor::convert_to_datetime(as.numeric(DATE)))

  } else{

    # if saved as a character string in Excel
    parse.orders <- c("ymd IMS p", "Ymd HM", "Ymd HMS")

    if(!is.na(suppressWarnings(parse_date_time(date_format, orders = parse.orders)))){

      sensor.data <- sensor.data %>%
        mutate(DATE = lubridate::parse_date_time(DATE, orders = parse.orders))

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

# data.char has three columns of class character: INDEX, DATE, PLACEHOLDER
# row1, row2, row3, and row4 are metadata to add to the DATE and PLACEHOLDER columns
# row4[1] is added to the DATE column; row4[2] is added to the PLACEHOLDER column

# data.char is returned with the additional rows

add_metadata <- function(data.char, row1, row2, row3, row4){

  data.char %>%
    add_row(INDEX = as.character(-1), DATE = row4[1], PLACEHOLDER = row4[2], .before = 1) %>%
    add_row(INDEX = as.character(-2), DATE = row3, PLACEHOLDER = row3, .before = 1) %>%
    add_row(INDEX = as.character(-3), DATE = row2, PLACEHOLDER = row2, .before = 1) %>%
    add_row(INDEX = as.character(-4), DATE = row1, PLACEHOLDER = row1, .before = 1)
}

