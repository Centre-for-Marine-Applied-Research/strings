#'@title Compiles temperature, dissolved oxygen, and salinity data from
#'  aquaMeasure deployment
#'@description This functions re-formats the data from an aquaMeasure deployment
#'  so it can be combined with the HOBO temperature data.
#'@details Negative DO values are replaced with \code{NA}.
#'
#'I haven't done anything with the DATES. Still need to un-account for
#'  daylight savings. To be discussed.
#'
#'  Parsing error is from the final line of the aquaMeasure data file.
#'@param path.aM File path to the aquaMeasure data. Should end with
#'  \code{/aquaMeasure}.
#'@param area.name Area where aquaMeasure was deployed.
#'@param vars.aM The variables to extract. Could possibly replace with
#'  unique(Record Type)
#'@param depth.aM The depth at which the sensor was deployed.
#'@param deployment.range The start and end dates of deployment from the
#'  deployment log. Must be in format "2018-Nov-15 to 2020-Jan-24".
#'
#'@return Returns a dataframe and exports a spreadsheet with the aquaMeasure
#'  data, including the appropriate metadata. Note that to include the metadata,
#'  all values were converted to class \code{character}. To manipulate the data,
#'  the values must be converted to the appropriate class (e.g., \code{POSIXct}
#'  for \code{DATE}, \code{numeric} for temperature values). This can be done
#'  using the function \code{convert_to_tidydata()}.
#'@family compile
#'@author Danielle Dempsey
#'@importFrom lubridate as_date parse_date_time
#'@importFrom readxl read_excel
#'@importFrom readr write_csv read_csv
#'@importFrom tidyr separate
#'@import dplyr
#'@export


compile_aquaMeasure_data <- function(path.aM, area.name, vars.aM = c("Temperature", "Dissolved Oxygen", "Salinity"),
                                     depth.aM, deployment.range){

  # initialize dateframe for storing the output
  aM_dat <- data.frame(INDEX = as.character())

  # list files in the data folder
  dat.files <- list.files(path.aM, all.files = FALSE)
  aM_dat_raw <- read_csv(paste(path.aM, dat.files[1], sep = "/"), col_names = TRUE)

  # Error message in case trying to extract a variable that is not in the dataset OR
  # a variable is spelled wrong
  if(any(!(vars.aM %in%  unique(aM_dat_raw$`Record Type`)))){

    stop("At least one of the variables in vars.aM is not in this dataframe. Check spelling in vars.aM")
  }

  # sensor and serial number
  serial <- aM_dat_raw$Sensor[1]

  # extract date column header (includes UTC offset)
  date_ref <- names(aM_dat_raw)[2]

  for(i in 1:length(vars.aM)){

  aM.i <- aM_dat_raw %>%
    select(`Timestamp(UTC)`, `Record Type`, vars.aM[i]) %>%
    filter(`Record Type` == vars.aM[i]) %>%
    rename(PLACEHOLDER = 3) %>%
    mutate(INDEX = as.character(c(1:n())))

  if(vars.aM[i] == "Dissolved Oxygen") aM.i <- aM.i %>% filter(PLACEHOLDER > 0)

  #mutate(DATE = convert_to_datetime(DATE)) %>%       # convert DATE to datetime
   # mutate(DATE = as.character(DATE)) %>%              # convert DATE to a character so can add in the column headings

  aM.i <- aM.i %>%
    mutate(DATE = parse_date_time(`Timestamp(UTC)`, orders = "Ymd HM")) %>%
    transmute(INDEX, DATE = as.character(DATE), PLACEHOLDER = as.character(PLACEHOLDER)) %>%
    # add meta data rows (deployment date, serial number, variable-depth can be merged in Excel file)
    # Date and Time stay unmerged
    add_row(INDEX = as.character(-1),
            DATE= date_ref, PLACEHOLDER = vars.aM[i], .before = 1) %>%
    add_row(INDEX = as.character(-2),
            DATE= paste(vars.aM[i], depth.aM, sep = "-"), PLACEHOLDER = paste(vars.aM[i], depth.aM, sep = "-"), .before = 1) %>%
    add_row(INDEX = as.character(-3),
            DATE= serial, PLACEHOLDER = serial, .before = 1) %>%
    add_row(INDEX = as.character(-4),
            DATE= deployment.range, PLACEHOLDER = deployment.range, .before = 1)

  # merge data on the INDEX row
  aM_dat <- full_join(aM_dat, aM.i, by = "INDEX")

  }

  # extract the deployment date from deployment.range
  file.date <- separate(data = data.frame(deployment.range),
                        col = deployment.range,
                        into = c("file.date", NA, NA), sep  = " " ) %>%
    as.character() %>%
    as_date() %>%
    format('%Y-%m-%d')

  # vars
  TEMP <- ifelse(any(vars.aM %in% "Temperature"), temp <- "_TEMP", "")
  DO <- ifelse(any(vars.aM %in% "Dissolved Oxygen"), "_DO", "")
  SAL <- ifelse(any(vars.aM %in% "Salinity"), "_SAL", "")

  # name of output file
  file.name <- paste(area.name, "_", file.date, TEMP, DO, SAL, sep = "")

  write_csv(aM_dat, path = paste(path.aM, "/", file.name, ".csv", sep = ""), col_names = FALSE)

}
