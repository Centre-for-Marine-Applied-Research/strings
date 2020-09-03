#'@title Formats temperature data from Vemco deployment
#'@description This function formats data from a Vemco deployment so it can be
#'  compiled with the HOBO and aquaMeasure data.
#'@details Can handle .csv and .xlsx files.
#'
#'  All columns are read in as class character to ensure the timestamp is parsed
#'  correctly. Timestamp must be saved in excel as a number or a character in
#'  the order "ymd IMS p", "Ymd IMS p", "Ymd HM", "Ymd HMS", "dmY HM", or "dmY
#'  HMS".
#'
#'  If there are "Temperature" entries in the Description column, these will be
#'  extracted and compiled. If there are no "Temperature" entries, but there are
#'  "Average temperature" entries, these will be extracted and compiled.
#'  Otherwise, the function will stop with an error message.
#'
#'@inheritParams compile_HOBO_data
#'@param path.vemco File path to the Vemco folder. This folder should have one
#'  csv file that was extracted using Vue software. Other file types in the
#'  folder will be ignored.
#'@param area.name Area where the Vemco was deployed.
#'@param depth.vemco Character string indicating the depth at which the Vemco
#'  was deployed, in the format "10m".
#'@return Returns a dataframe or exports a spreadsheet with the formatted Vemco
#'  data in two columns: the timestamp (in the format "Y-m-d H:M:S") and
#'  temperature value (degree celsius, rounded to three decimal places).
#'  Metadata at the top of each column indicates the deployment period, the
#'  sensor serial number, the depth of the sensor, and the timezone of the
#'  timestamp.
#'
#'  To include the metadata, all values were converted to class
#'  \code{character}. To manipulate the data, the values must be converted to
#'  the appropriate class (e.g., \code{POSIXct} for the timestamps and
#'  \code{numeric} for temperature values). This can be done using the function
#'  \code{convert_to_tidydata()}.
#'@family compile
#'@author Danielle Dempsey
#'
#'@importFrom lubridate parse_date_time
#'@importFrom readr read_csv write_csv
#'@importFrom readxl read_excel
#'@import dplyr
#'@export
#'

compile_vemco_data <- function(path.vemco,
                              area.name,
                              depth.vemco,
                              deployment.range,
                              trim = TRUE,
                              export.csv = FALSE){


  # extract the deployment start and end dates from deployment.range
  dates <- extract_deployment_dates(deployment.range)
  start.date <- dates$start
  end.date <- dates$end

# List file to be compiled -----------------------------------------------

  # finish path
  path.vemco <- file.path(paste(path.vemco, "Vemco", sep = "/"))

  # list csv and xlsx files in the data folder
  dat.file <- list.files(path.vemco, all.files = FALSE, pattern = "*csv|*xlsx")

  # remove files that start with "~"
  if(any(substring(dat.file, 1, 1)== "~")) {

    message(paste("Note:", sum((substring(dat.files, 1, 1)== "~")),
                "files on the path begin with ~ and were not imported.", sep = " "))
    dat.files <- dat.files[-which(substring(dat.files, 1, 1)== "~")]

  }

  if(length(dat.file) > 1) warning("More than one file found in path/Vemco. Only the first will be imported")

  # check whether file is .csv or .xlsx
  file.extension <- separate(data.frame(dat.file), col = dat.file,
                               into = c(NA, "EXT"), sep = "\\.")
  file.extension <- file.extension$EXT

  # use appropriate function to import data
  if(file.extension == "csv") {
    vemco_dat <- read_csv(paste(path.vemco,  dat.file[1], sep = "/"),
                          col_names = TRUE,
                          col_types = cols(.default = col_character()))
  }

  if(file.extension == "xlsx") {

    vemco_dat <- read_excel(paste(path.vemco,  dat.file[1], sep = "/"),
                          col_names = TRUE,
                          col_types = "text")

  }

  # Extract metadata --------------------------------------------------------

  # sensor and serial number
  serial <- vemco_dat$Receiver[1]

  # extract date column header (includes UTC offset)
  date_ref <- names(vemco_dat)[1]

  # format deployment date range for metadata
  deployment_ref <- paste(format(start.date, "%Y-%b-%d"), "to", format(end.date, "%Y-%b-%d"))


  # Format data -------------------------------------------------------------

  description.col <- unique(vemco_dat$Description)

  if("Temperature" %in% description.col) {
    var.to.extract = "Temperature"
  } else if("Average temperature" %in% description.col){
    var.to.extract = "Average temperature"
  } else stop("Could not find Temperature or Average temperature in vemco_dat. Check file.")

  # select the first three columns
  vemco <- vemco_dat %>%
    filter(Description == var.to.extract) %>%
    transmute(INDEX = c(1:n()),
              TIMESTAMP = `Date and Time (UTC)`,
              TEMPERATURE = Data) %>%
    convert_timestamp_to_datetime()

  # trim to the dates in deployment.range
  # added four hours to end.date to account for AST
  # (e.g., in case the sensor was retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
  if(trim == TRUE) {
    vemco <- vemco %>%
      filter(TIMESTAMP >= start.date, TIMESTAMP <= (end.date + hours(4))) %>%
      mutate(INDEX = c(1:n()))
  }

  # convert columns to class character so can add in the meta data
  vemco <- vemco %>%
    mutate(INDEX = as.character(round(as.numeric(INDEX), digits = 0)), # make sure INDEX will have the same class and format for each sheet
           TIMESTAMP = format(TIMESTAMP,  "%Y-%m-%d %H:%M:%S"),
           PLACEHOLDER = as.character(round(as.numeric(TEMPERATURE), digits = 3)))  %>%
    select(INDEX, TIMESTAMP, PLACEHOLDER) %>%
    add_metadata(row1 = deployment_ref,
                 row2 = serial,
                 row3 = paste("Temperature", depth.vemco, sep = "-"),
                 row4 = c(date_ref, "Temperature"))


# Return compiled data ----------------------------------------------------

  if(export.csv == TRUE){
    # format start date for file name
    file.date <-  format(start.date, '%Y-%m-%d')

    # name of output file
    file.name <- paste(area.name, file.date, sep = "_")

    write_csv(vemco, path = paste(path.vemco, "/", file.name, ".csv", sep = ""), col_names = FALSE)

    print(paste("Check in ", path.vemco, " for file ", file.name, ".csv", sep = ""))

  } else{

    print(paste("Vemco data compiled:", var.to.extract))

    vemco
  }


}

