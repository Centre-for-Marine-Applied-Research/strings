#'@title Compiles temperature, dissolved oxygen, and/or salinity data from
#'  aquaMeasure sensors
#'@description Compiles and formats data from aquaMeasure sensors.
#'@details The raw aquaMeasure data must be saved in a folder named aquaMeasure
#'  in .csv or .xlsx format.
#'
#'  Rows with \code{undefined} and \code{... (time not set)} values in the
#'  \code{Timestamp(UTC)} column are filtered out.
#'
#'  Negative Dissolved Oxygen values are replaced with \code{NA}.
#'
#'  All columns in are imported as characters to ensure the timestamp is parsed
#'  correctly. Timestamp must be saved in excel as a number or a character in
#'  the order "ymd IMS p", "Ymd IMS p", "Ymd HM", "Ymd HMS", "dmY HM", or "dmY
#'  HMS".
#'
#'  There still may be parsing errors because there are not entries in every
#'  column. This should not affect the data compilation. To check, save the
#'  spreadsheet with a new name new, delete the column causing the error (likley
#'  the "Text" column), re-run the function, and verify that there is no parsing
#'  error.
#'
#'@inheritParams compile_HOBO_data
#'@param path.aM File path to the aquaMeasure folder.
#'@param serial.table.aM A table with the serial number of each aquaMeasure on
#'  the string, in the form "aquaMeasure-xxxxxx" (first column; note the capital
#'  "M") and its corresponding depth in the form "2m" (second column).
#'@return Returns a dataframe or exports a spreadsheet with the data compiled
#'  from each of the aquaMeasure sensors. Columns alternate between timestamp
#'  (UTC, in the format "Y-m-d H:M:S") and variable value (rounded to three
#'  decimal places). Metadata at the top of each column indicates the deployment
#'  and retrieval dates, the sensor serial number, the variable and depth of the
#'  sensor, and the timezone of the timestamp.
#'
#'  To include the metadata, all values were converted to class
#'  \code{character}. To manipulate the data, the values must be converted to
#'  the appropriate class (e.g., \code{POSIXct} for the timestamps and
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
  names(serial.table.aM) <- c("SENSOR", "DEPTH")

  # extract the deployment start and end dates from deployment.range
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

    message(paste("Note:", sum((substring(dat.files, 1, 1)== "~")),
                "files on the path begin with ~ and were not imported.", sep = " "))
    dat.files <- dat.files[-which(substring(dat.files, 1, 1)== "~")]

  }

# Import data -------------------------------------------------------------

  # loop over each aM file
  for(i in 1:length(dat.files)) {

    # check whether file is .csv or .xlsx
    file.i <- dat.files[i]
    file.type.i <- extract_file_extension(file.i)

    # use appropriate function to import data
    if(file.type.i == "csv") {
      dat.i <- read_csv(paste(path.aM, file.i, sep = "/"),
                             col_names = TRUE,
                             col_types = cols(.default = col_character()))
    }

    if(file.type.i == "xlsx") {
      dat.i <- read_excel(paste(path.aM, file.i, sep = "/"),
                               col_names = TRUE,
                               col_types = "text")
    }


    # Extract metadata --------------------------------------------------------

    # sensor and serial number
    sensor.i <- dat.i$Sensor[1]

    # use serial number to identify the depth (from serial.table)
    depth.i <- serial.table.aM %>%
      dplyr::filter(SENSOR == sensor.i)  %>%
      select(DEPTH)
    depth.i <- depth.i$DEPTH

    # if the name of the file doesn't match any of the entries in serial.table: stop with message
    if(!(sensor.i %in% serial.table.aM$SENSOR)){
      stop(paste("Serial number", sensor.i, "does not match any serial numbers in serial.table.aM"))
    }

    # extract date column header (includes UTC offset)
    # use pattern match for "stamp" to look for column named "timestamp"
    date_ref.i <- names(dat.i)[grep("stamp", names(dat.i))]

    # format deployment date range for metadata
    deployment_ref <- paste(format(start.date, "%Y-%b-%d"), "to", format(end.date, "%Y-%b-%d"))

    # Clean and format data ---------------------------------------------------
    if("Temperature" %in% colnames(dat.i) & "Temp(Water)" %in% colnames(dat.i)){
      warning("There is a column named Temperature and a column named Temp(Water) in sensor ",
              sensor.i)
    }

    # Re-name the "Temp(Water)" column to "Temperature"
    if(!("Temperature" %in% colnames(dat.i)) & "Temp(Water)" %in% colnames(dat.i)){
      dat.i <- dat.i %>% rename(Temperature = `Temp(Water)`)
    }

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
      select(TIMESTAMP = `Timestamp(UTC)`, `Record Type`, all_of(vars.to.select)) %>%
      mutate(DATE_VALUES = str_detect(TIMESTAMP, "(time not set)")) %>%
      filter(TIMESTAMP != "undefined", DATE_VALUES == FALSE) %>%
      select(-DATE_VALUES) %>%
      convert_timestamp_to_datetime()  # convert the timestamp to a POSIXct object

    # trim to the dates in deployment.range
    # added four hours to end.date to account for AST
    # (e.g., in case the sensor was retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
    if(trim == TRUE) {
      dat.i <- dat.i %>%
        filter(TIMESTAMP >= start.date, TIMESTAMP <= (end.date + hours(4)))
    }

    for(j in 1:length(vars.to.select)){

      var.j <- vars.to.select[j]

      aM.j <- dat.i %>%
        select(TIMESTAMP, `Record Type`, all_of(var.j)) %>%
        filter(`Record Type` == var.j) %>%
        rename(PLACEHOLDER = 3)

      if(nrow(aM.j) > 0) {

        aM.j <- aM.j %>%
          mutate(INDEX = as.character(c(1:n())),
                 PLACEHOLDER = round(as.numeric(PLACEHOLDER), digits = 3))

        if(var.j == "Dissolved Oxygen") aM.j <- aM.j %>% filter(PLACEHOLDER > 0)

        aM.j <- aM.j %>%
          transmute(INDEX,
                    TIMESTAMP = as.character(TIMESTAMP),
                    PLACEHOLDER = as.character(PLACEHOLDER)) %>%
          add_metadata(row1 = deployment_ref,
                       row2 = sensor.i,
                       row3 = paste(var.j, depth.i, sep = "-"),
                       row4 = c(date_ref.i, var.j))

        # merge data on the INDEX row
        aM_dat <- full_join(aM_dat, aM.j, by = "INDEX")
      }  else message(paste("No", var.j, "observations found for", sensor.i))


  } # end loop over variables


} # end loop over files

# Return compiled data ----------------------------------------------------

  if(export.csv == TRUE){

    # format start date for file name
    file.date <-  format(start.date, '%Y-%m-%d')

    # name of output file
    file.name <- name_compiled_data(area.name = area.name,
                                    deployment.start = file.date,
                                    vars = unique(convert_to_tidydata(aM_dat[,-1])$VARIABLE))

    write_csv(aM_dat, path = paste(path.aM, "/", file.name, ".csv", sep = ""), col_names = FALSE)

    print(paste("Check in ", path.aM, " for file ", file.name, ".csv", sep = ""))
  }else{

    print("aquaMeasure data compiled")

    aM_dat
  }

}
