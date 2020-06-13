#'@title Compiles HOBO, aquaMeasure, and Vemco data from a single deployment
#'@details Write that it called the other functions and to look at those for help
#'@inheritParams compile_HOBO_data
#'@inheritParams compile_aquaMeasure_data
#'@inheritParams compile_vemco_data
#'@param path File path to the Hobo, aquaMeasure, and/or Vemco folders.
#'@return Returns a dataframe or exports a spreadsheet with the formatted Vemco
#'  data in two columns: the timestamp (UTC, in the format "Y-m-d H:M:S") and
#'  temperature value (degree celsius, rounded to three decimal places).
#'  Metadata at the top of each column indicates the deployment range, the
#'  sensor serial number, and the depth of the sensor. Each datetime column
#'  shows the timezone as extracted from the Vue software.
#'
#'  To include the metadata, all values were converted to class
#'  \code{character}. To manipulate the data, the values must be converted to
#'  the appropriate class (e.g., \code{POSIXct} for the datetimes and
#'  \code{numeric} for temperature values). This can be done using the function
#'  \code{convert_to_tidydata()}.
#'@family compile
#'@author Danielle Dempsey
#'
#'@importFrom lubridate parse_date_time
#'@importFrom readr read_csv write_csv
#'@import dplyr
#'@export
#'
compile_all_data <- function(path,
                             area.name,
                             deployment.range,
                             trim,
                             # hobo
                             serial.table = NULL,
                             file.type = NULL,
                             # aquaMeaure
                             vars.aM = NULL,
                             depth.aM = NULL,
                             # vemco
                             depth.vemco = NULL){

  ALL <- data.frame(INDEX = as.character())

  # compile HOBO data
  if(length(serial.table) > 0){

    HOBO <- compile_HOBO_data(path.HOBO = path,
                              area.name = area.name,
                              serial.table = serial.table,
                              deployment.range = deployment.range,
                              trim = trim,
                              file.type = file.type)

    ALL <- full_join(ALL, HOBO, by = "INDEX")
  }


  # compile aquaMeasure data
  if(length(depth.aM) > 0){

    aM <- compile_aquaMeasure_data(path.aM = path,
                                   area.name = area.name,
                                   vars.aM = vars.aM,
                                   depth.aM = depth.aM,
                                   deployment.range = deployment.range,
                                   trim = TRUE)

    ALL <- full_join(ALL, aM, by = "INDEX")
  }

  # compile Vemco data
  if(length(depth.vemco) > 0){

    vemco <- compile_vemco_data(path.vemco = path,
                                area.name = area.name,
                                depth.vemco = depth.vemco,
                                deployment.range = deployment.range,
                                trim = TRUE)

    ALL <- full_join(ALL, vemco, by = "INDEX")
  }

  # remove index column
  ALL <- ALL %>%
    select(-"INDEX")


}

