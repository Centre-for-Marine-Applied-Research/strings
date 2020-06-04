#'@title Compiles temperature, dissolved oxygen, and salinity data from
#'  aquaMeasure deployment
#'@description This functions re-formats the data from an aquaMeasure deployment
#'  so it can be combined with the HOBO temperature data.
#'@details Rows with \code{undefined} and \code{... (time not set)} values in
#'  the Timestamp(UTC) column are filtered out.
#'
#'  Negative DO values are replaced with \code{NA}.
#'
#'@param path.aM File path to the aquaMeasure folder. There should only be one
#'  file in the aquaMeasure folder. A warning will be printed to the console if
#'  there is more than one file. The function can accept .csv or .xlsx files.
#'@param area.name Area where aquaMeasure was deployed.
#'@param vars.aM The variables to extract. (Could possibly replace with
#'  unique(Record Type))
#'@param depth.aM The depth at which the sensor was deployed.
#'@inheritParams compile_HOBO_data
#'@return Returns a dataframe and exports a spreadsheet with the aquaMeasure
#'  data, including the appropriate metadata. Note that to include the metadata,
#'  all values were converted to class \code{character}. To manipulate the data,
#'  the values must be converted to the appropriate class (e.g., \code{POSIXct}
#'  for \code{DATE}, \code{numeric} for temperature values). This can be done
#'  using the function \code{convert_to_tidydata()}.
#'@family compile
#'@author Danielle Dempsey
#'@importFrom janitor convert_to_datetime
#'@importFrom lubridate as_date parse_date_time
#'@importFrom readxl read_excel
#'@importFrom readr write_csv read_csv
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

  # extract the deployment start and end dates from deployment.range
  start_end_date <- separate(data = data.frame(deployment.range),
                             col = deployment.range,
                             into = c("start.date", NA, "end.date"), sep  = " " )

  start.date <- as_datetime(paste(start_end_date$start.date, "00:00:00"))
  end.date <- as_datetime(paste(start_end_date$end.date, "23:59:59"))

  # initialize dateframe for storing the output
  aM_dat <- data.frame(INDEX = as.character())

  # finish path
  path.aM <- file.path(paste(path.aM, "/aquaMeasure", sep = ""))

  # list files in the data folder
  dat.files <- list.files(path.aM, all.files = FALSE)

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
    aM_dat_raw <- read_csv(paste(path.aM, dat.files, sep = "/"), col_names = TRUE)
  }

  if(file.extension == "xlsx") {
    aM_dat_raw <- read_excel(paste(path.aM, dat.files, sep = "/"), col_names = TRUE)
  }

  # Error message in case trying to extract a variable that is not in the dataset OR
  # a variable is spelled wrong
  if(any(!(vars.aM %in%  unique(aM_dat_raw$`Record Type`)))){

    stop("At least one of the variables in vars.aM is not in this dataframe. Check spelling in vars.aM")
  }

  # sensor and serial number
  serial <- aM_dat_raw$Sensor[1]

  # extract date column header (includes UTC offset)
  date_ref <- names(aM_dat_raw)[2]

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
      mutate(INDEX = as.character(c(1:n())))

    if(vars.aM[i] == "Dissolved Oxygen") aM.i <- aM.i %>% filter(PLACEHOLDER > 0)

    aM.i <- aM.i %>%
      transmute(INDEX, DATE = as.character(DATE), PLACEHOLDER = as.character(PLACEHOLDER)) %>%
      add_metadata(row1 = deployment.range,
                   row2 = serial,
                   row3 = paste(vars.aM[i], depth.aM, sep = "-"),
                   row4 = c(date_ref, vars.aM[i]))

    # merge data on the INDEX row
    aM_dat <- full_join(aM_dat, aM.i, by = "INDEX")

  } # end for loop


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

    print("Note: to export csv file, set export.csv = TRUE")

    aM_dat
  }

}
