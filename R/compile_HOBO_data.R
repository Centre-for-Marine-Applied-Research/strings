#'@title Compiles temperature data from HOBO deployment
#'@description This function compiles the data from a HOBO deployment at
#'  different depths into a single dataframe or spreadsheet.
#'@details
#'  HOBO data should be exported in GMT+00. Note that the HOBO
#'  timestamp accounts for daylight savings time, but true UTC does not. This
#'  function gives the option to convert to true UTC time with the
#'  \code{real_UTC} argument.
#'
#'  The functions used to convert to true UTC are
#'  \code{convert_HOBO_datetime_to_real_UTC()} and \code{dates_to_fix()}, which
#'  are NOT exported to the \code{strings} package (i.e., they are only used
#'  internally).
#'
#'  \code{convert_HOBO_datetime_to_real_UTC()} identifies which times are during
#'  daylight savings by creating two additional columns: \code{ADT_force =
#'  force_tz(DATE, tzone = "America/Halifax")}, and \code{DAYLIGHT_SAVINGS =
#'  dst(ADT_force)}. Where \code{DAYLIGHT_SAVINGS == TRUE}, the \code{DATE} is
#'  shifted back by 1 hour.
#'
#'  This leaves apparent duplications for the hour of 1 am on the day that
#'  daylight savings ends. \code{dates_to_fix()} identifies these \code{n}
#'  obervations (e.g. 1:00, 1:15, 1:30, 1:45, 1:00, 1:15, 1:30, 1:45), and
#'  shifts the first \code{n/2} back by one hour (e.g. 00:00, 00:15, 00:30,
#'  00:45, 1:00, 1:15, 1:30, 1:45). Function can handing this from 2017 - 2021;
#'  additional lines required for other years.
#'
#'  If for some reason, there is an ODD number of duplicates, the function might
#'  break.
#'
#'@param path.HOBO File path to the Hobo folder. All of the excel files in the
#'  Hobo folder should be data extracted from the HOBO software. The name of
#'  each excel file must be the serial number of the sensor.
#'@param area.name Area where the HOBO was deployed.
#'@param serial.table A table with the serial number of each HOBO (first column)
#'  and corresponding depth at which it was deployed (second column).
#'@param deployment.range The start and end dates of deployment from the
#'  deployment log. Must be in format "2018-Nov-15 to 2020-Jan-24".
#'@param trim Logical value indicating whether to trim the data to the dates
#'  specified in \code{deployment.range}. (Note: four hours are added to the
#'  retrieval date to account for AST, e.g., in case the sensor was retrieved
#'  after 20:00 AST, which is 00:00 UTC the next day.) Default is \code{trim = TRUE}.
#'@param tz.UTC Logical value indicating whether to convert timestamp to UTC.
#'  Note that the HOBO timestamp GMT+00 accounts for daylight savings time, but
#'  true UTC does not. \code{true_UTC = FALSE} will return the timestamps as
#'  exported from the HOBO software. \code{true_UTC = TRUE} converts the
#'  timestamp to true UTC time. Default is \code{real_UTC = TRuE}.
#'@param export.csv Logical value indicating whether to export the compiled data
#'  as a .csv file. If \code{export.csv = TRUE}, the compiled data will not be
#'  returned to the global environment. Default is \code{export.csv = FALSE}.
#'@return Exports a dataframe or spreadsheet with the HOBO temperature data,
#'  including the appropriate metadata. Note that to include the metadata, all
#'  values were converted to class \code{character}. To manipulate the data, the
#'  values must be converted to the appropriate class (e.g., \code{POSIXct} for
#'  \code{DATE}, \code{numeric} for temperature values). This can be done using
#'  the function \code{convert_to_tidydata()}.
#'@family compile
#'@author Danielle Dempsey
#'@importFrom janitor convert_to_datetime
#'@importFrom lubridate as_datetime
#'@importFrom readxl read_excel
#'@importFrom readr write_csv
#'@importFrom tidyr separate
#'@import dplyr
#'@export


compile_HOBO_data <- function(path.HOBO,
                              area.name,
                              serial.table,
                              deployment.range,
                              trim = TRUE,
                              tz.UTC = TRUE,
                              export.csv = FALSE){

  names(serial.table) <- c("SERIAL", "VAR_DEPTH")

  # extract the deployment start and end dates from deployment.range
  start_end_date <- separate(data = data.frame(deployment.range),
                             col = deployment.range,
                             into = c("start.date", NA, "end.date"), sep  = " " )

  start.date <- as_datetime(paste(start_end_date$start.date, "00:00:00"))
  end.date <- as_datetime(paste(start_end_date$end.date, "23:59:59"))

  # initialize dateframe for storing the output
  HOBO_dat <- data.frame(INDEX = as.character())

  # finish path
  path.HOBO <- file.path(paste(path.HOBO, "/Hobo", sep = ""))

  # list files .xlsx files in the data folder
  dat.files <- list.files(path.HOBO, all.files = FALSE, pattern = "*.xlsx")

  # loop over each HOBO file
  for(i in 1:length(dat.files)) {

    # import HOBO file i
    hobo.i_dat <- read_excel(paste(path.HOBO, dat.files[i], sep = "/"), col_names = FALSE)

    # extract serial number
    serial.i <- hobo.i_dat[1,1] %>%
      tidyr::separate(col = 1, into = c(NA, "SERIAL"), sep = ": ", remove = TRUE)
    serial.i <- paste("HOBO-", serial.i$SERIAL, sep = "")

    # use serial number to identify the variable and depth (from serial.table)
    variable_depth <- serial.table %>%
      dplyr::filter(SERIAL == serial.i)  %>%
      select(VAR_DEPTH)
    variable_depth <- variable_depth$VAR_DEPTH

    # remove plot title row and select the first three columns
    hobo.i <- hobo.i_dat %>%
      slice(-1) %>%
      select(c(1:3))

    # extract date column header (includes GMT offset)
    date_ref <- hobo.i[1,2]$...2
    # extract temperature column header (includes units)
    temp_ref <- data.frame(hobo.i[1,3]$...3) %>%
      rename("temp_ref" = 1) %>%
      separate(col = "temp_ref", into = c("temp_ref", NA), sep = 8)
    temp_ref <- temp_ref$temp_ref

    # format data
    hobo.i <- hobo.i %>%
      slice(-1) %>%                                                     # remove column headings
      select(INDEX = 1, DATE = 2, TEMPERATURE = 3) %>%                  # rename columns (will be dropped for export)
      mutate(DATE = convert_to_datetime(DATE),                          # convert DATE to datetime
             TEMPERATURE = round(as.numeric(TEMPERATURE), digits = 3))  # round temperature data to 3 decimal places

    # un-account for daylight savings time
    # (subtract 1 hour from each datetime within the range of DST)
    if(tz.UTC == TRUE) hobo.i <- hobo.i %>% convert_HOBO_datetime_to_true_UTC()

    # trim to the dates in deployment.range
    # added four hours to end.date to account for AST
    # (e.g., in case the sensor was retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
    if(trim == TRUE) {
      hobo.i <- hobo.i %>%
        filter(DATE >= start.date, DATE <= (end.date + hours(4)))
    }

    # convert columns to class character so can add in the column headings
    hobo.i <- hobo.i %>%
      mutate(DATE = as.character(DATE),
             INDEX = as.character(round(as.numeric(INDEX), digits = 0)), # make sure INDEX will have the same class and format for each sheet
             PLACEHOLDER = as.character(TEMPERATURE))  %>%
      select(-TEMPERATURE) %>%
      add_metadata(row1 = deployment.range, row2 = serial.i, row3 = variable_depth, row4 = c(date_ref, temp_ref))

    # merge data on the INDEX row
    HOBO_dat <- full_join(HOBO_dat, hobo.i, by = "INDEX")

  } # end for loop

  if(export.csv == TRUE){
    # format start date for file name
    file.date <-  format(start.date, '%Y-%m-%d')

    # name of output file
    file.name <- paste(area.name, file.date, sep = "_")

    write_csv(HOBO_dat, path = paste(path.HOBO, "/", file.name, ".csv", sep = ""), col_names = FALSE)

    print(paste("Check in ", path.HOBO, " for file ", file.name, ".csv", sep = ""))
  } else{

    print("Note: to export csv file, set export.csv = TRUE")

    HOBO_dat
  }


}

