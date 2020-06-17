#'@title Compiles HOBO, aquaMeasure, and Vemco data from a single deployment
#'@details Calls \code{compile_HOBO_data()}, \code{compile_aquaMeasure_data()}
#'  and \code{compile_vemco_data()} and joins the results into a single
#'  dataframe.
#'
#'  HOBO data must be in a folder named Hobo, aquaMeasure data must be in a
#'  folder named aquaMeasure, and Vemco data must be in a folder name Vemco. The
#'  Hobo, aquaMeasure, and Vemco folders must be in the same folder.
#'
#'  If one type of sensor was not included in the deployment, set the
#'  argument(s) related to that sensor to \code{NULL} (the default). For
#'  example, if there is no aquaMeaure data for the deployment, set
#'  \code{serial.table.aM = NULL}
#'@inheritParams compile_HOBO_data

#'@param path File path to the Hobo, aquaMeasure, and/or Vemco folders.
#'@param area.name Area where the sensor string was deployed.
#'@param serial.table.HOBO A table with the serial number of each HOBO sensor on
#'  the string, in the form "HOBO-xxxxxxxx" (first column) and corresponding
#'  variable it measured at the depth it was deployed in the form
#'  "Temperature-2m" (second column). Default is \code{serial.table.HOBO =
#'  NULL}.
#'@param file.type.HOBO Character string indicating whether the HOBO data is in
#'  .csv or .xlsx format. All HOBO data files that are being compiled must have
#'  the same file extension.  Default is \code{file.type.HOBO = NULL}. Alternatives
#'  are \code{file.type = "csv"} and \code{file.type = "xlsx"}.
#'
#'  Note that the HOBO software exports csv files in true UTC, but xlsx files
#'  account for daylight savings time. This function will automatically convert
#'  the datetimes in xlsx files to true UTC. Future versions of the package will
#'  provide an option to convert from ADT to AST.
#'@param serial.table.aM A table with the serial number of each aquaMeasure on
#'  the string, in the form "aquaMeasure-xxxxxx" (first column; note the capital
#'  "M") and its corresponding depth in the form "2m" (second column). Default
#'  is \code{serial.table.aquaMeasure = NULL}.
#'@param depth.vemco Character string indicating the depth at which the Vemco
#'  was deployed, in the form "10m". Default is \code{depth.vemco = NULL}.
#'@return Returns a dataframe with the compiled and formatted data. Columns
#'  alternate between the timestamp (in the format "Y-m-d H:M:S") and the
#'  variable value (rounded to three decimal places). Metadata at the top of
#'  each column indicates the deployment dates, the sensor serial number, and
#'  the variable and depth of the sensor. Each datetime column shows the
#'  timezone as extracted from the sensor.
#'
#'  To include the metadata, all values were converted to class
#'  \code{character}. To manipulate the data, the values must be converted to
#'  the appropriate class (e.g., \code{POSIXct} for the datetimes and
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
                             area.name,
                             deployment.range,
                             trim,
                             # hobo
                             serial.table.HOBO = NULL,
                             file.type.HOBO = NULL,
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
                              trim = trim,
                              file.type = file.type.HOBO)

    ALL <- full_join(ALL, HOBO, by = "INDEX")
  }


  # compile aquaMeasure data
  if(length(serial.table.aM) > 0){

    aM <- compile_aquaMeasure_data(path.aM = path,
                                   area.name = area.name,
                                   serial.table.aM = serial.table.aM,
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

