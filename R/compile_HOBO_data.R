#'@title Compiles temperature data from HOBO deployment
#'@description This function compiles the data from a HOBO deployment at
#'  different depths into a single dataframe or spreadsheet.
#'@details
#'       # add wanring if dup datetimes
#'       could make date format an argument
#'HOBO data should be exported in GMT+00. Note that the HOBO timestamp
#'  accounts for daylight savings time, but true UTC does not. This function
#'  gives the option to convert to true UTC time with the \code{real_UTC}
#'  argument.
#'
#'  The functions used to convert to true UTC are
#'  \code{convert_HOBO_datetime_to_real_UTC()} and \code{dates_to_fix()}, which
#'  are NOT exported to the \code{strings} package (i.e., they are only used
#'  internally).
#'
#'  \code{convert_HOBO_datetime_to_true_UTC()} identifies which times are during
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
#'  each excel file must be the serial number of the sensor, and the excel files
#'  must all have the same extension (either .csv or .xlsx)
#'@param area.name Area where the HOBO was deployed.
#'@param serial.table A table with the serial number of each HOBO (first column)
#'  and corresponding depth at which it was deployed (second column).
#'@param deployment.range The start and end dates of deployment from the
#'  deployment log. Must be in format "2018-Nov-15 to 2020-Jan-24".
#'@param trim Logical value indicating whether to trim the data to the dates
#'  specified in \code{deployment.range}. (Note: four hours are added to the
#'  retrieval date to account for AST, e.g., in case the sensor was retrieved
#'  after 20:00 AST, which is 00:00 UTC the next day.) Default is \code{trim =
#'  TRUE}.
#'@param file.type Character string indicating whether the HOBO data is in .csv
#'  or .xlsx format. All data files that are being compiled must have the same
#'  file extension. Note that the HOBO software exports csv files in true UTC,
#'  but xlsx files account for daylight savings time. This function will
#'  automatically convert the datetimes in xlsx files to true UTC. Default is
#'  \code{file.type = "csv"}. Alternative is \code{file.type = "xlsx"}.
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
                              file.type = "csv",
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


# List files to be compiled -----------------------------------------------

  # finish path
  path.HOBO <- file.path(paste(path.HOBO, "/Hobo", sep = ""))

  # list files .xlsx files in the data folder
  if(file.type == "xlsx"){
    dat.files <- list.files(path.HOBO, all.files = FALSE, pattern = "*.xlsx")
    tz.UTC <- TRUE
  }

  # list files .csv files in the data folder
  if(file.type == "csv"){
    dat.files <- list.files(path.HOBO, all.files = FALSE, pattern = "*.csv")
    tz.UTC <- FALSE
  }

  # remove files that start with "~"
  if(any(substring(dat.files, 1, 1)== "~")) {

    dat.files <- dat.files[-which(substring(dat.files, 1, 1)== "~")]
    print(paste("Note:", sum((substring(dat.files, 1, 1)== "~")),
                "files on the path begin with ~ and were not imported.", sep = " "))
  }


  # loop over each HOBO file
  for(i in 1:length(dat.files)) {


# Import Data -------------------------------------------------------------

    file.name <- dat.files[i]

    # import HOBO file i. Remove the first row if the first cell is the Plot Title
    if(file.type == "xlsx") {
      hobo.i_dat <- read_excel(paste(path.HOBO, file.name, sep = "/"), col_names = FALSE)

      if(hobo.i[1,1] != "#")  hobo.i <- hobo.i_dat %>% slice(-1)
    }
    if(file.type == "csv") {
      hobo.i_dat <- read_csv(paste(path.HOBO,  file.name, sep = "/"), col_names = FALSE, skip = 1)

      if(hobo.i_dat[1,1] != "#") {
        hobo.i_dat <- read_csv(paste(path.HOBO,  file.name, sep = "/"), col_names = FALSE)
      }
    }

    # select the first three columns
    hobo.i <- hobo.i_dat %>%
      select(c(1:3))


# Extract metadata --------------------------------------------------------

    # extract serial number from file name
    serial.i <- data.frame(file.name) %>%
      tidyr::separate(col = file.name, into = c("SERIAL", NA), sep = "\\.", remove = TRUE)
    serial.i <- paste("HOBO-", serial.i$SERIAL, sep = "")

    # use serial number to identify the variable and depth (from serial.table)
    variable_depth <- serial.table %>%
      dplyr::filter(SERIAL == serial.i)  %>%
      select(VAR_DEPTH)
    variable_depth <- variable_depth$VAR_DEPTH

    # extract date column header (includes GMT offset)
    #date_ref <- hobo.i[1,2]$...2
    date_ref <- hobo.i[1,2]$X2
    # extract temperature column header (includes units)
    temp_ref <- data.frame(hobo.i[1,3]) %>%
      rename("temp_ref" = 1) %>%
      separate(col = "temp_ref", into = c("temp_ref", NA), sep = 8)
    temp_ref <- temp_ref$temp_ref


# Format data -------------------------------------------------------------

    hobo.i <- hobo.i %>%
      slice(-1) %>%                                       # remove column headings
      select(INDEX = 1, DATE = 2, TEMPERATURE = 3)        # rename columns (will be dropped for export)

    # if the date can be converted to class numeric, then it is stored as a number in Excel
    ## and we have to use janitor::convert_to_datetime to convert to POSIXct.
    # Otherwise the date should be a character string that can be converted to POSIXct using
    ## lubridate::parse_date_time()

    date_format <- hobo.i$DATE[1]
    if(!is.na(suppressWarnings(as.numeric(date_format)))) {

      hobo.i <- hobo.i %>%
        mutate(DATE = convert_to_datetime(as.numeric(DATE)))

    } else{

      hobo.i <- hobo.i %>%
        mutate(DATE = parse_date_time(DATE,
                                      orders = c("ymd IMS p", "Ymd HM", "Ymd HMS")))
    }

    #hobo.i <- hobo.i %>%
     # mutate(DATE = format(DATE,  "%Y-%m-%d %H:%M:%S"),                # format date to "2020-06-09 20:16:30"
      #       TEMPERATURE = round(as.numeric(TEMPERATURE), digits = 3)) # round temperature data to 3 decimal places


    # un-account for daylight savings time
    # (subtract 1 hour from each datetime within the range of DST)
    if(tz.UTC == TRUE) hobo.i <- hobo.i %>% convert_HOBO_datetime_to_true_UTC()

    # trim to the dates in deployment.range
    # added four hours to end.date to account for AST
    # (e.g., in case the sensor was retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
    if(trim == TRUE) {
      hobo.i <- hobo.i %>%
        filter(DATE >= start.date, DATE <= (end.date + hours(4))) %>%
        mutate(INDEX = c(1:n()))
    }

    # convert columns to class character so can add in the meta data
    hobo.i <- hobo.i %>%
      mutate(INDEX = as.character(round(as.numeric(INDEX), digits = 0)), # make sure INDEX will have the same class and format for each sheet
             DATE = format(DATE,  "%Y-%m-%d %H:%M:%S"),
             PLACEHOLDER = as.character(round(as.numeric(TEMPERATURE), digits = 3)))  %>%
      select(INDEX, DATE, PLACEHOLDER) %>%
      add_metadata(row1 = deployment.range, row2 = serial.i, row3 = variable_depth, row4 = c(date_ref, temp_ref))

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

    print("Note: to export csv file, set export.csv = TRUE")

    HOBO_dat
  }


}

