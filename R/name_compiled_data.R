#'@title Generates name for output file of compiled data
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

  # vars
  TEMP <- ifelse(any(vars %in% "Temperature"), temp <- "_TEMP", "")
  DO <- ifelse(any(vars %in% "Dissolved Oxygen"), "_DO", "")
  SAL <- ifelse(any(vars %in% "Salinity"), "_SAL", "")

  # name of output file
  paste(area.name, "_", deployment.start, TEMP, DO, SAL, sep = "")

}



#'@title Generates name for output file for Open Data portal
#'@description Generates a name for output file in the format "area
#'  name deployment date", e.g. "Shad Bay 2018-11-15".
#'@param name.input File name as exported from \code{name_compiled_data()}, e.g. "Shad Bay_2018-11-15_TEMP_DO"
#'@return Character string to use to name Open Data file.
#'@importFrom tidyr separate
#'@export

name_for_open_data <- function(name.input){

  open.data.name <- name.input %>%
    data.frame() %>% rename(STATION = 1) %>%
    separate(STATION, into = c("STATION", "START_DAY"), sep = "_", extra = "drop") %>%
    mutate(FILENAME = paste(STATION, START_DAY))

  open.data.name$FILENAME
}
