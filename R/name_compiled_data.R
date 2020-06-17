#'@title Generates name for output file
#'@description Generates a name for output file in the format "area
#'  name_deployment date_variables", e.g. "Shad Bay_2018-11-15_TEMP_DO".
#'@inheritParams compile_aquaMeasure_data
#'@param vars The variables included in the data file, e.g. \code{vars =
#'  c("Temperature", "Dissolved Oxygen")}.
#'@importFrom tidyr separate
#'@importFrom lubridate as_date
#'@export

name_compiled_data <- function(area.name, deployment.range, vars){

  # format start date for file name
  file.date <- separate(data = data.frame(deployment.range), col = deployment.range,
                        into = c("file.date", NA, NA), sep  = " " ) %>%
    as.character() %>%
    as_date() %>%
    format('%Y-%m-%d')

  # vars
  TEMP <- ifelse(any(vars %in% "Temperature"), temp <- "_TEMP", "")
  DO <- ifelse(any(vars %in% "Dissolved Oxygen"), "_DO", "")
  SAL <- ifelse(any(vars %in% "Salinity"), "_SAL", "")

  # name of output file
  paste(area.name, "_", file.date, TEMP, DO, SAL, sep = "")

}
