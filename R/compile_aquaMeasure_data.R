#'@title Compiles temperature, dissolved oxygen, and/or salinity data from
#'  aquaMeasure deployment
#'@description This functions re-formats the data from an aquaMeasure deployment
#'  so it can be combined with the HOBO and Vemco temperature data.
#'@details Might be able to get rid of parsing error by deleting Text column. IF
#'  you do this, make another folder inside aquaMeaure named Raw Data and save
#'  the unaltered data there
#'
#'  Can handle .csv or .xlsx files.
#'
#'  All columns in .xlsx files will be imported as characters to ensure dates
#'  are parsed correctly.
#'
#'  The first 7 columns in .csv files will be imported as characters. There
#'  still may be parsing errors because there are not entries in every column.
#'
#'  Rows with \code{undefined} and \code{... (time not set)} values in the
#'  \code{Timestamp(UTC)} column are filtered out.
#'
#'  Negative Dissolved Oxygen values are replaced with \code{NA}.
#'
#'@inheritParams compile_HOBO_data
#'@param path.aM File path to the aquaMeasure folder. There should only be one
#'  file in the aquaMeasure folder. A warning will be printed to the console if
#'  there is more than one file. The function can accept .csv or .xlsx files.
#'@param area.name Area where aquaMeasure was deployed.
#'@param serial.table.aM A table with the serial number of each aquaMeasure on the
#'  string, in the form "aquaMeasure-xxxxxx" (first column; note the capital
#'  "M") and its corresponding depth in the form "2m" (second column).
#'@return Returns a dataframe or exports a spreadsheet with the data compiled
#'  from each of the aquaMeasure sensors. Columns alternate between datetime
#'  (UTC, in the format "Y-m-d H:M:S") and variable value (rounded to three
#'  decimal places). Metadata at the top of each column indicates the deployment
#'  range, the sensor serial number, and the variable and depth of the sensor.
#'  Each datetime column shows the timezone as extracted from the aquaMeasure.
#'
#'  To include the metadata, all values were converted to class
#'  \code{character}. To manipulate the data, the values must be converted to
#'  the appropriate class (e.g., \code{POSIXct} for the datetimes and
#'  \code{numeric} for variable values). This can be done using the function
#'  \code{convert_to_tidydata()}.
#'@family compile
#'@author Danielle Dempsey
#'@importFrom janitor convert_to_datetime
#'@importFrom lubridate parse_date_time
#'@importFrom readxl read_excel
#'@importFrom readr write_csv read_csv cols col_character
#'@importFrom stringr str_detect
#'@importFrom tidyr separate
#'@importFrom tidyselect all_of
#'@import dplyr
#'@export


compile_aquaMeasure_data <- function(path.aM,
                                     area.name,
                                     serial.table.aM,
                                     deployment.range,
                                     trim = TRUE,
                                     export.csv = FALSE){

  # make sure columns of serial.table are named correctly
  names(serial.table.aM) <- c("SERIAL", "DEPTH")

  # extract the deployment start and end dates from deployment.dates
  dates <- extract_deployment_dates(deployment.range)
  start.date <- dates$start
  end.date <- dates$end

  # initialize dateframe for storing the output
  aM_dat <- data.frame(INDEX = as.character())

# List files to be compiled -----------------------------------------------

  # finish path
  path.aM <- file.path(paste(path.aM, "/aquaMeasure", sep = ""))

  # list csv and xlsx files in the data folder
  dat.files <- list.files(path.aM, all.files = FALSE, pattern = "*csv|*xlsx")

  # remove files that start with "~"
  if(any(substring(dat.files, 1, 1)== "~")) {

    dat.files <- dat.files[-which(substring(dat.files, 1, 1)== "~")]
    print(paste("Note:", sum((substring(dat.files, 1, 1)== "~")),
                "files on the path begin with ~ and were not imported.", sep = " "))
  }

# Import data -------------------------------------------------------------

  # loop over each aM file
  for(i in 1:length(dat.files)) {

    # check whether file is .csv or .xlsx
    file.i <- dat.files[i]
    file.extension.i <- separate(data.frame(file.i), col = file.i,
                               into = c(NA, "EXT"), sep = "\\.")
    file.extension.i <- file.extension.i$EXT

    # use appropriate function to import data
    if(file.extension.i == "csv") {
      dat.i <- read_csv(paste(path.aM, file.i, sep = "/"),
                             col_names = TRUE,
                             col_types = cols(.default = col_character()))
    }

    if(file.extension.i == "xlsx") {
      dat.i <- read_excel(paste(path.aM, file.i, sep = "/"),
                               col_names = TRUE,
                               col_types = "text")
    }


    # Extract metadata --------------------------------------------------------

    # sensor and serial number
    serial.i <- dat.i$Sensor[1]

    # use serial number to identify the depth (from serial.table)
    depth.i <- serial.table.aM %>%
      dplyr::filter(SERIAL == serial.i)  %>%
      select(DEPTH)
    depth.i <- depth.i$DEPTH

    # if the name of the file doesn't match any of the entries in serial.table: stop with message
    if(!(serial.i %in% serial.table.aM$SERIAL)){
      stop(paste("Serial number", serial.i, "does not match any serial numbers in serial.table.aM"))
    }

    # extract date column header (includes UTC offset)
    # use pattern match for "stamp" to look for column named "timestamp"
    date_ref.i <- names(dat.i)[grep("stamp", names(dat.i))]


    # Clean and format data ---------------------------------------------------

    ## check colnames of dat.i for "Temperature", "Dissolved Oxygen", and "Salinity"
    temp <- ifelse("Temperature" %in% colnames(dat.i), "Temperature", NA)
    DO <- ifelse("Dissolved Oxygen" %in% colnames(dat.i), "Dissolved Oxygen", NA)
    sal <- ifelse("Salinity" %in% colnames(dat.i), "Salinity", NA)

    # create vector of the variables in this file by removing NA
    vars.to.select <- c(temp, DO, sal)
    vars.to.select <- vars.to.select[which(!is.na(vars.to.select))]


    print(paste("found", vars.to.select, "in file", file.i), sep = " ")


    # filter out DATES that sensor was not set up
    # "undefined" or "(time not set)"
    dat.i <- dat.i %>%
      select(DATE = `Timestamp(UTC)`, `Record Type`, all_of(vars.to.select)) %>%
      mutate(DATE_VALUES = str_detect(DATE, "(time not set)")) %>%
      filter(DATE != "undefined", DATE_VALUES == FALSE) %>%
      select(-DATE_VALUES) %>%
      convert_timestamp_to_datetime()  # convert the timestamp to a POSIXct object

    # trim to the dates in deployment.range
    # added four hours to end.date to account for AST
    # (e.g., in case the sensor was retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
    if(trim == TRUE) {
      dat.i <- dat.i %>%
        filter(DATE >= start.date, DATE <= (end.date + hours(4)))
    }

    for(j in 1:length(vars.to.select)){

      var.j <- vars.to.select[j]

      aM.j <- dat.i %>%
        select(DATE, `Record Type`, all_of(var.j)) %>%
        filter(`Record Type` == var.j) %>%
        rename(PLACEHOLDER = 3) %>%
        mutate(INDEX = as.character(c(1:n())),
               PLACEHOLDER = round(as.numeric(PLACEHOLDER), digits = 3))

      if(var.j == "Dissolved Oxygen") aM.j <- aM.j %>% filter(PLACEHOLDER > 0)

      aM.j <- aM.j %>%
        transmute(INDEX,
                  DATE = as.character(DATE),
                  PLACEHOLDER = as.character(PLACEHOLDER)) %>%
        add_metadata(row1 = deployment.range,
                     row2 = serial.i,
                     row3 = paste(var.j, depth.i, sep = "-"),
                     row4 = c(date_ref.i, var.j))

      # merge data on the INDEX row
      aM_dat <- full_join(aM_dat, aM.j, by = "INDEX")

    } # end loop over variables


  } # end loop over files

# Return compiled data ----------------------------------------------------

  if(export.csv == TRUE){
    # # format start date for file name
    # file.date <-  format(start.date, '%Y-%m-%d')
    #
    # # vars
    # TEMP <- ifelse(any(vars.aM %in% "Temperature"), temp <- "_TEMP", "")
    # DO <- ifelse(any(vars.aM %in% "Dissolved Oxygen"), "_DO", "")
    # SAL <- ifelse(any(vars.aM %in% "Salinity"), "_SAL", "")
    #
    # # name of output file
    # file.name <- paste(area.name, "_", file.date, TEMP, DO, SAL, sep = "")
    #
    # write_csv(aM_dat, path = paste(path.aM, "/", file.name, ".csv", sep = ""), col_names = FALSE)
    #
    # print(paste("Check in ", path.aM, " for file ", file.name, ".csv", sep = ""))
  }else{

    print("aquaMeasure data compiled")

    aM_dat
  }

}
