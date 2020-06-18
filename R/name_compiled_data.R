#'@title Generates name for output file
#'@description Generates a name for output file in the format "area
#'  name_deployment date_variables", e.g. "Shad Bay_2018-11-15_TEMP_DO".
#'@inheritParams compile_aquaMeasure_data
#'@param deployment.start start date of deployment
#'@param vars The variables included in the data file, e.g. \code{vars =
#'  c("Temperature", "Dissolved Oxygen")}.
#'@importFrom tidyr separate
#'@importFrom lubridate as_date
#'@export

name_compiled_data <- function(area.name, deployment.start, vars){

  # format start date for file name
 # file.date <-

  # vars
  TEMP <- ifelse(any(vars %in% "Temperature"), temp <- "_TEMP", "")
  DO <- ifelse(any(vars %in% "Dissolved Oxygen"), "_DO", "")
  SAL <- ifelse(any(vars %in% "Salinity"), "_SAL", "")

  # name of output file
  paste(area.name, "_", deployment.start, TEMP, DO, SAL, sep = "")

}
