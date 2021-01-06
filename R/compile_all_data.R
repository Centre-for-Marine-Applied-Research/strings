#'@title Compiles HOBO, aquaMeasure, and Vemco data from a single deployment
#'@details Calls \code{compile_HOBO_data()}, \code{compile_aquaMeasure_data()}
#'  and \code{compile_vemco_data()} and joins the results into a single wide
#'  dataframe.
#'
#'  HOBO data must be in a folder named Hobo, aquaMeasure data must be in a
#'  folder named aquaMeasure, and Vemco data must be in a folder name Vemco. The
#'  Hobo, aquaMeasure, and Vemco folders must be in the same folder.
#'
#'  If one type of sensor was not included in the deployment, set the argument
#'  related to that sensor to \code{NULL} (the default). For example, if there
#'  is no aquaMeaure data for the deployment, set \code{serial.table.aM = NULL}
#'
#'@inheritParams compile_HOBO_data
#'@inheritParams compile_aquaMeasure_data
#'@inheritParams compile_vemco_data

#'@param path File path to the Hobo, aquaMeasure, and/or Vemco folders.
#'@param area.name Area where the sensor string was deployed.
#'@return Returns a dataframe with the compiled and formatted data. Columns
#'  alternate between the timestamp (in the format "Y-m-d H:M:S") and the
#'  variable value (rounded to three decimal places). Metadata at the top of
#'  each column indicates the deployment and retrieval dates, the sensor serial
#'  number, and the variable and depth of the sensor. Each timestamp column
#'  shows the timezone as extracted from the sensor.
#'
#'  To include the metadata, all values were converted to class
#'  \code{character}. To manipulate the data, the values must be converted to
#'  the appropriate class (e.g., \code{POSIXct} for the timestamp and
#'  \code{numeric} for variable values). This can be done using the function
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
                             area.name = "",
                             deployment.range,
                             trim = TRUE,
                             # hobo
                             serial.table.HOBO = NULL,
                             # aquaMeaure
                             serial.table.aM = NULL,
                             # vemco
                             depth.vemco = NULL){

  ALL <- data.frame(INDEX = as.character())

  # compile HOBO data
  if(length(serial.table.HOBO) > 0){

    HOBO <- compile_HOBO_data(path.HOBO = path,
                              area.name = area.name,
                              serial.table.HOBO = serial.table.HOBO,
                              deployment.range = deployment.range,
                              trim = trim)

    ALL <- full_join(ALL, HOBO, by = "INDEX")
  }


  # compile aquaMeasure data
  if(length(serial.table.aM) > 0){

    aM <- compile_aquaMeasure_data(path.aM = path,
                                   area.name = area.name,
                                   serial.table.aM = serial.table.aM,
                                   deployment.range = deployment.range,
                                   trim = trim)

    ALL <- full_join(ALL, aM, by = "INDEX")
  }

  # compile Vemco data
  if(length(depth.vemco) > 0){

    vemco <- compile_vemco_data(path.vemco = path,
                                area.name = area.name,
                                depth.vemco = depth.vemco,
                                deployment.range = deployment.range,
                                trim = trim)

    ALL <- full_join(ALL, vemco, by = "INDEX")
  }

  # remove index column
  ALL <- ALL %>%
    select(-"INDEX")

}

