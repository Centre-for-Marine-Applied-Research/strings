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
#'@param vars.aM The variables to extract. Default is \code{vars.aM =
#'  c("Temperature", "Dissolved Oxygen", "Salinity")}.
#'@param depth.aM Character string describing the depth at which the sensor was
#'  deployed, in the format "10m".
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
#'@import dplyr
#'@export


compile_aquaMeasure_data <- function(path.aM,
                                     area.name,
                                     vars.aM = c("Temperature", "Dissolved Oxygen", "Salinity"),
                                     depth.aM,
                                     deployment.range,
                                     trim = TRUE,
                                     export.csv = FALSE){

  # initialize dateframe for storing the output
  aM_dat <- data.frame(INDEX = as.character())

  # extract the deployment start and end dates from deployment.dates
  dates <- extract_deployment_dates(deployment.range)
  start.date <- dates$start
  end.date <- dates$end

# Import data -------------------------------------------------------------

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

  if(length(dat.files) > 1){
    print(paste("Warning: there is more than one file in ", path.aM, ". Only the first file will be imported.", sep = ""))
  }

  # check whether file is .csv or .xlsx
  dat.files <- dat.files[1]
  file.extension <- separate(data.frame(dat.files), col = dat.files,
                             into = c(NA, "EXT"), sep = "\\.")
  file.extension <- file.extension$EXT

  # use appropriate function to import data
  if(file.extension == "csv") {
    aM_dat_raw <- read_csv(paste(path.aM, dat.files, sep = "/"),
                           col_names = TRUE,
                           col_types = cols(.default = col_character()))
  }

  if(file.extension == "xlsx") {
    aM_dat_raw <- read_excel(paste(path.aM, dat.files, sep = "/"),
                             col_names = TRUE,
                             col_types = "text")
  }

  # Error message in case trying to extract a variable that is not in the dataset OR
  # a variable is spelled wrong
  if(any(!(vars.aM %in%  unique(aM_dat_raw$`Record Type`)))){

    stop("At least one of the variables in vars.aM is not in this dataframe. Check spelling in vars.aM")
  }


# Extract metadata --------------------------------------------------------

  # sensor and serial number
  serial <- aM_dat_raw$Sensor[1]

  # extract date column header (includes UTC offset)
  date_ref <- names(aM_dat_raw)[2]


# Clean and format data ---------------------------------------------------

  # filter out DATES that sensor was not set up
  # "undefined" or "(time not set)"
  aM_dat_raw <- aM_dat_raw %>%
    select(DATE = `Timestamp(UTC)`, `Record Type`, `Dissolved Oxygen`, Temperature) %>%
    mutate(DATE_VALUES = str_detect(DATE, "(time not set)")) %>%
    filter(DATE != "undefined", DATE_VALUES == FALSE) %>%
    select(-DATE_VALUES)

  # if the date can be converted to class numeric, then it is stored as a number in Excel
  ## and we have to use janitor::convert_to_datetime to convert to POSIXct.
  # Otherwise the date should be a character string that can be converted to POSIXct using
  ## lubridate::parse_date_time
  date_format <- aM_dat_raw$DATE[1]
  if(!is.na(suppressWarnings(as.numeric(date_format)))) {

    aM_dat_raw <- aM_dat_raw %>%
      mutate(DATE = convert_to_datetime(as.numeric(DATE)))
  } else{

    aM_dat_raw <- aM_dat_raw %>%
      mutate(DATE = parse_date_time(DATE,
                                    orders = c("Ymd HM", "Ymd HMS")))
  }

  # trim to the dates in deployment.range
  # added four hours to end.date to account for AST
  # (e.g., in case the sensor was retrieved after 20:00 AST, which is 00:00 UTC **The next day**)
  if(trim == TRUE) {
    aM_dat_raw <- aM_dat_raw %>%
      filter(DATE >= start.date, DATE <= (end.date + hours(4)))
  }

  for(i in 1:length(vars.aM)){

    aM.i <- aM_dat_raw %>%
      select(DATE, `Record Type`, vars.aM[i]) %>%
      filter(`Record Type` == vars.aM[i]) %>%
      rename(PLACEHOLDER = 3) %>%
      mutate(INDEX = as.character(c(1:n())),
             PLACEHOLDER = round(as.numeric(PLACEHOLDER), digits = 3))

    if(vars.aM[i] == "Dissolved Oxygen") aM.i <- aM.i %>% filter(PLACEHOLDER > 0)

    aM.i <- aM.i %>%
      transmute(INDEX,
                DATE = as.character(DATE),
                PLACEHOLDER = as.character(PLACEHOLDER)) %>%
      add_metadata(row1 = deployment.range,
                   row2 = serial,
                   row3 = paste(vars.aM[i], depth.aM, sep = "-"),
                   row4 = c(date_ref, vars.aM[i]))

    # merge data on the INDEX row
    aM_dat <- full_join(aM_dat, aM.i, by = "INDEX")

  } # end for loop


# Return compiled data ----------------------------------------------------

  if(export.csv == TRUE){
    # format start date for file name
    file.date <-  format(start.date, '%Y-%m-%d')

    # vars
    TEMP <- ifelse(any(vars.aM %in% "Temperature"), temp <- "_TEMP", "")
    DO <- ifelse(any(vars.aM %in% "Dissolved Oxygen"), "_DO", "")
    SAL <- ifelse(any(vars.aM %in% "Salinity"), "_SAL", "")

    # name of output file
    file.name <- paste(area.name, "_", file.date, TEMP, DO, SAL, sep = "")

    write_csv(aM_dat, path = paste(path.aM, "/", file.name, ".csv", sep = ""), col_names = FALSE)

    print(paste("Check in ", path.aM, " for file ", file.name, ".csv", sep = ""))
  }else{

    print("aquaMeasure data compiled")

    aM_dat
  }

}
