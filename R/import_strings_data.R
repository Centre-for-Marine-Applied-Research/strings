#'@title Import string data
#'@details More about the path here
#'@param input_path Placeholder
#'@param output_path Path to where assembled file should be exported
#'@param county Optional filter
#'@param export_csv Logical argument indicating whether the data
#'  should be exported as a *.csv file.
#'@param return_global Logical argument indicating whether the assembled data
#'  should be returned to the global environment. Default is \code{FALSE}.
#'
#'@family OpenData
#'@author Danielle Dempsey
#'
#'@importFrom purrr map_dfr
#'@importFrom data.table fwrite
#'@importFrom stringr str_subset
#'@importFrom googlesheets4 gs4_deauth read_sheet
#'@import dplyr
#'@export

import_strings_data <- function(input_path = NULL,
                                output_path = "",
                                county = "ALL",
                                add_county_col = TRUE,
                                export_csv = FALSE,
                                return_global = TRUE) {

  message("importing ", county, "data...")

  # Input path --------------------------------------------------------------

  if(is.null(input_path)){
    # path to Open Data folder
    input_path <- "Y:/Coastal Monitoring Program/Open Data/County Datasets"

  } else input_path <- input_path

  # list rds files on the path and import -----------------------------------

  dat <- list.files(input_path,
                    full.names = TRUE,
                    pattern = ".rds")

  # filter for specified county(ies)
  if(county != "ALL") dat <- dat %>% str_subset(county)

  # read and bind the rds files
  dat <- dat %>%
    purrr::map_dfr(readRDS)

  # merge with Area_Info table to add COUNTY column
  if(add_county_col == TRUE){

    # allow access to the google sheet
    googlesheets4::gs4_deauth()

    # link to the "STRING TRACKING" google sheet
    link <- "https://docs.google.com/spreadsheets/d/1a3QvJsvwr4dd64g3jxgewRtfutIpsKjT2yrMEAoxA3I/edit#gid=828367890"

    # read in the "Area Info" tab of the STRING TRACKING sheet
    Area_Info <- googlesheets4::read_sheet(link, sheet = "Area Info") %>%
      rename(COUNTY = County, STATION = Station, WATERBODY = Waterbody)

    # check if there are any WATERBODYs or STATIONs in dat that are NOT in Area_Info
    if(any(!(unique(dat$WATERBODY) %in% Area_Info$WATERBODY))){

      waterbody_distinct <- dat %>% distinct(WATERBODY)  %>%
        filter(!(WATERBODY %in% Area_Info$WATERBODY))

      warning("Found WATERBODY in county data that is not in Area_Info: ", waterbody_distinct$WATERBODY)
    }

    if(any(!(unique(dat$STATION) %in% Area_Info$STATION))){

      station_distinct <- dat %>% distinct(STATION)  %>%
        filter(!(STATION %in% Area_Info$STATION))

      warning("Found STATION in county data that is not in Area_Info: ", station_distinct$STATION)
    }


    # merge dat and Area_Info
    dat <- left_join(dat, Area_Info, by = c("WATERBODY", "STATION")) %>%
      select(COUNTY, WATERBODY, STATION, everything())

  }


  # export as csv and/or return to global environment -----------------------

  if(export_csv == TRUE){

    message("exporting data for ",  county)
    # today's date (for file name)
    today_date <- Sys.Date()

    data.table::fwrite(dat, file = output_path, showProgress = TRUE)
  }

  if(return_global == TRUE) dat
}
