#'@title Format sensor string data for the OpenData portal
#'@param dat.tidy Data from a single deployment in tidy format, as returned by
#'  the function \code{convert_to_tidydata()}. Must include four columns:
#'  \code{DATE} (POSIXct), \code{VARIABLE} (character), \code{DEPTH} (ordered
#'  factor), and \code{VALUE} (numeric).
#'@param lat.deploy The latitude at which the station was deployed.
#'@param long.deploy The longitude at which the station was deployed.
#'@param waterbody The water body in which the station was deployed.
#'@param county The county in which the station was deployed.

#'@return Returns a dataframe of tidy data in the format for the OpenData portal for a single deployment,

#'@family format
#'@author Danielle Dempsey
#'
#'@importFrom tidyr separate
#'@import dplyr
#'@export

format_for_opendata <- function(dat.tidy, lat.deploy, long.deploy, waterbody, county) {

  dat.tidy %>%
    separate(SENSOR, into = c("SENSOR", NA), sep = "-| - ") %>%
    mutate(LATITUDE = lat, LONGITUDE = long, WATERBODY = waterbody, COUNTY = COUNTY)


}
