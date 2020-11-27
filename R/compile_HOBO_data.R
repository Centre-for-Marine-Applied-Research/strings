#'@title Compiles temperature data from HOBO and TidbiT sensors
#'@description Compiles and formats data from HOBO and TidbiT sensors deployed
#'  on the same string.
#'@details HOBO and TidBiT data must be saved in a folder named Hobo in .csv or
#'  .xlsx format.
#'
#'  All columns are read in as characters to ensure the timestamp is parsed
#'  correctly. Timestamp must be saved in Excel as a number or a character in
#'  the order ""ymd IMS p", "Ymd IMS p", "Ymd HM", "Ymd HMS", "dmY HM", or "dmY
#'  HMS".
#'
#'  CMAR NOTES: Data should be exported from the Hobo software in GMT+00 as a
#'  csv file so that the timestamp is in UTC.
#'
#'  If exported as an xlsx, the timestamp accounts for daylight savings time
#'  (this seems to be a bug in the HOBO software). \code{compile_HOBO_data()}
#'  can convert xlsx files to true UTC by setting \code{rm.DST = TRUE}. This
#'  will subtract 1 hour from each datetime that occurs during daylight savings.
#'
#'  The functions used to convert to true UTC are
#'  \code{convert_HOBO_datetime_to_true_UTC()} and \code{dates_to_fix()}, which
#'  are NOT exported to the \code{strings} package (i.e., they are only used
#'  internally).
#'
#'  \code{convert_HOBO_datetime_to_true_UTC()} identifies which datetimes are
#'  during daylight savings by creating two additional columns: \code{ADT_force
#'  = force_tz(TIMESTAMP, tzone = "America/Halifax")}, and
#'  \code{DAYLIGHT_SAVINGS = dst(ADT_force)}. Where \code{DAYLIGHT_SAVINGS ==
#'  TRUE}, the \code{TIMESTAMP} is shifted back by 1 hour.
#'
#'  This leaves apparent duplicates for the hour of 1 am on the day that
#'  daylight savings ends. \code{dates_to_fix()} identifies these \code{n}
#'  observations (e.g. 1:00, 1:15, 1:30, 1:45, 1:00, 1:15, 1:30, 1:45), and
#'  shifts the first \code{n/2} back by one hour (e.g. 00:00, 00:15, 00:30,
#'  00:45, 1:00, 1:15, 1:30, 1:45).The function can handing this from 2015 -
#'  2021; additional lines required for other years.
#'
#'  If for some reason, there is an ODD number of duplicates, the function might
#'  break.
#'
#'@param path.HOBO File path to the Hobo folder. All of the excel files in the
#'  Hobo folder will be compiled. The name of each file must be the serial
#'  number of the sensor, and the excel files must be in either .csv or .xlsx
#'  format. The timestamp columns must be in the order "ymd IMS p", "Ymd IMS p",
#'  "Ymd HM", "Ymd HMS", "dmY HM", or "dmY HMS".
#'@param area.name Area where the sensor was deployed.
#'@param serial.table.HOBO A table with the serial number of each HOBO and
#'  TidBiT sensor on the string, in the form "HOBO-xxxxxxxx" or
#'  "TidbiT-xxxxxxxx" (first column) and corresponding depth at which it was
#'  deployed in the form "2m" (second column).
#'@param deployment.range A dataframe with two columns. The first column holds
#'  the deployment date (a Date object in the order year, month, day),  and the
#'  second column holds the retrieval date (a Date object in the order year,
#'  month, day).
#'@param trim Logical value indicating whether to trim the data to the dates
#'  specified in \code{deployment.range}. (Note: four hours are added to the
#'  retrieval date to account for AST, e.g., in case the sensor was retrieved
#'  after 20:00 AST, which is 00:00 UTC the next day.) Default is \code{trim =
#'  TRUE}.
#'@param rm.DST Option to remove daylight savings time. Here, daylight savings
#'  is as defined for Nova Scotia, Canada (begins in March and end in November).
#'  Only works for 2015 - 2021. It is strongly recommended that the data is in
#'  the correct timezone before using this function, and that the default
#'  \code{rm.DST = FALSE} is used.
#'@param export.csv Logical value indicating whether to export the compiled data
#'  as a .csv file. If \code{export.csv = TRUE}, the compiled data will not be
#'  returned to the global environment. Default is \code{export.csv = FALSE}.
#'@return Returns a dataframe or exports a spreadsheet with the data compiled
#'  from each of the HOBO and TidbiT sensors. Columns alternate between
#'  timestamp (in the format "Y-m-d H:M:S") and temperature value (rounded to
#'  three decimal places). Metadata at the top of each column indicates the
#'  deployment and retrieval dates, the sensor serial number, the depth of the
#'  sensor, the temperature units, and the timezone of the timestamp.
#'
#'  To include the metadata, all values were converted to class
#'  \code{character}. To manipulate the data, the values must be converted to
#'  the appropriate class (e.g., \code{POSIXct} for the timestamps and
#'  \code{numeric} for temperature values). This can be done using the function
#'  \code{convert_to_tidydata()}.
#'@family compile
#'@author Danielle Dempsey
#'
#'@importFrom janitor convert_to_datetime
#'@importFrom lubridate as_datetime
#'@importFrom readxl read_excel
#'@importFrom readr read_csv write_csv cols col_character
#'@importFrom tidyr separate
#'@import dplyr
#'@export


compile_HOBO_data <- function(path.HOBO,
                              area.name,
                              serial.table.HOBO,
                              deployment.range,
                              trim = TRUE,
                              rm.DST = FALSE,
                              export.csv = FALSE){

  # make sure columns of serial.table are named correctly
  names(serial.table.HOBO) <- c("SENSOR", "DEPTH")
  # separate the SENSOR column into the SENSOR type and SERIAL number
  serial.table.HOBO <- serial.table.HOBO %>%
    separate(col = SENSOR, into = c("SENSOR", "SERIAL"))

  # extract the deployment start and end dates from deployment.range
  dates <- extract_deployment_dates(deployment.range)
  start.date <- dates$start
  end.date <- dates$end

  # initialize dateframe for storing the output
  HOBO_dat <- data.frame(INDEX = as.character())


# List files to be compiled -----------------------------------------------

  # finish path
  path.HOBO <- file.path(paste(path.HOBO, "Hobo", sep = "/"))

  # list files in the Hobo folder
  dat.files <- list.files(path.HOBO, all.files = FALSE, pattern = "*xlsx|*csv")


  # loop over each HOBO file
  for(i in 1:length(dat.files)) {

# Import Data -------------------------------------------------------------

    file.name <- dat.files[i]

    # extract the file extension
    file.type <- extract_file_extension(file.name)

    # import HOBO file i. Remove the first row if the first cell is the Plot Title
    if(file.type == "xlsx") {
      hobo.i_dat <- read_excel(paste(path.HOBO, file.name, sep = "/"),
                               col_names = FALSE,
                               col_types = "text")

      if(hobo.i_dat[1,1] != "#")  hobo.i_dat <- hobo.i_dat %>% slice(-1)
    }
    if(file.type == "csv") {
      hobo.i_dat <- read_csv(paste(path.HOBO,  file.name, sep = "/"),
                             col_names = FALSE, skip = 1,
                             col_types = cols(.default = col_character()))

      if(hobo.i_dat[1,1] != "#") {
        hobo.i_dat <- read_csv(paste(path.HOBO,  file.name, sep = "/"),
                               col_names = FALSE,
                               col_types = cols(.default = col_character()))
      }
    }

    # select the first three columns
    hobo.i <- hobo.i_dat %>%
      select(c(1:3))

# Extract metadata --------------------------------------------------------

    # extract serial number from file name
    serial.i <- data.frame(file.name) %>%
      tidyr::separate(col = file.name, into = c("SERIAL", NA), sep = "\\.", remove = TRUE)
    serial.i <- serial.i$SERIAL

    # if the name of the file doesn't match any of the entries in serial.table.HOBO: stop with message
    if(!(serial.i %in% serial.table.HOBO$SERIAL)){
      stop(paste("The name of file", i, "does not match any serial numbers in serial.table.HOBO"))
    }

    # use serial number to identify the variable and depth (from serial.table)
    depth <- serial.table.HOBO %>%
      dplyr::filter(SERIAL == serial.i)  %>%
      select(DEPTH)
    depth <- depth$DEPTH

    # sensor type and serial number
    sensor.i <- paste(serial.table.HOBO$SENSOR[i], serial.table.HOBO$SERIAL[i], sep = "-")

    # extract date column header (includes GMT offset)
    if(file.type == "xlsx") date_ref <- hobo.i[1,2]$...2
    if(file.type == "csv") date_ref <- hobo.i[1,2]$X2

    # extract temperature column header (includes units)
    temp_ref <- data.frame(hobo.i[1,3]) %>%
      rename("temp_ref" = 1) %>%
      separate(col = "temp_ref", into = c("temp_ref", NA), sep = 8)
    temp_ref <- temp_ref$temp_ref

    # format deployment date range for metadata
    deployment_ref <- paste(format(start.date, "%Y-%b-%d"), "to", format(end.date, "%Y-%b-%d"))


# Format data -------------------------------------------------------------

    hobo.i <- hobo.i %>%
      slice(-1) %>%                                       # remove column headings
      select(INDEX = 1, TIMESTAMP = 2, TEMPERATURE = 3) %>%    # rename columns (will be dropped for export)
      convert_timestamp_to_datetime()                     # convert the timestamp to a POSIXct object

    # un-account for daylight savings time
    # (subtract 1 hour from each datetime within the range of DST)
    if(rm.DST == TRUE) hobo.i <- hobo.i %>% convert_HOBO_datetime_to_true_UTC()

    # trim to the dates in deployment.range
    # added four hours to end.date to account for AST
    # (e.g., in case the sensor was retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
    if(trim == TRUE) {
      hobo.i <- hobo.i %>%
        filter(TIMESTAMP >= start.date, TIMESTAMP <= (end.date + hours(4))) %>%
        mutate(INDEX = c(1:n()))
    }

    # convert columns to class character so can add in the meta data
    hobo.i <- hobo.i %>%
      mutate(INDEX = as.character(round(as.numeric(INDEX), digits = 0)), # make sure INDEX will have the same class and format for each sheet
             TIMESTAMP = format(TIMESTAMP,  "%Y-%m-%d %H:%M:%S"),
             PLACEHOLDER = as.character(round(as.numeric(TEMPERATURE), digits = 3)))  %>%
      select(INDEX, TIMESTAMP, PLACEHOLDER) %>%
      add_metadata(row1 = deployment_ref,
                   row2 = sensor.i,
                   row3 = (paste("Temperature", depth, sep = "-")),
                   row4 = c(date_ref, temp_ref))

    # merge data on the INDEX row
    HOBO_dat <- full_join(HOBO_dat, hobo.i, by = "INDEX")

  } # end for loop


# Return compiled data ----------------------------------------------------

  if(export.csv == TRUE){
    # format start date for file name
    file.date <-  format(start.date, '%Y-%m-%d')

    # name of output file
    file.name <- paste(area.name, file.date, sep = "_")

    write_csv(HOBO_dat, path = paste(path.HOBO, "/", file.name, ".csv", sep = ""), col_names = FALSE)

    print(paste("Check in ", path.HOBO, " for file ", file.name, ".csv", sep = ""))

  } else{

    print("HOBO data compiled")

    HOBO_dat
  }


}

