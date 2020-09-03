#'@title Format sensor string data for the OpenData portal
#'@param dat.tidy Data from a single deployment in tidy format, as returned by
#'  the function \code{convert_to_tidydata()}. Must include four columns:
#'  \code{TIMESTAMP} (POSIXct), \code{VARIABLE} (character), \code{DEPTH} (ordered
#'  factor), and \code{VALUE} (numeric).
#'@param location.info Dataframe of information about the deployment location. One
#'  observation of 5 columns: \code{county}, \code{waterbody}, \code{station}, \code{lease},
#'  \code{latitude}, and \code{longitude}.

#'@return Returns a dataframe of tidy data in the format for the OpenData portal for a single deployment,

#'@family format OpenData
#'@author Danielle Dempsey
#'
#'@importFrom tidyr separate
#'@import dplyr
#'@export

format_for_opendata <- function(dat.tidy, location.info) {

  dat.tidy %>%
    #separate(SENSOR, into = c("SENSOR", NA), sep = "-| - ") %>%
    mutate(COUNTY = location.info$county,
           WATERBODY = location.info$waterbody,
           STATION = location.info$station,
           LEASE = as.character(location.info$lease),
           LATITUDE = location.info$latitude,
           LONGITUDE = location.info$longitude,
           TIMESTAMP = format(TIMESTAMP, "%Y-%m%-%d %H:%M:%S")) %>%
    mutate(LEASE = if_else(LEASE == "n/a" | LEASE == "N/A" , "NA", LEASE)) %>%
    select(WATERBODY, STATION, LEASE,
           LATITUDE, LONGITUDE, DEPLOYMENT_PERIOD,
           TIMESTAMP, SENSOR, DEPTH, VARIABLE, VALUE)

}
