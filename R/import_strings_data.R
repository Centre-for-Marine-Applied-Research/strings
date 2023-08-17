#' @title Import string data from rds files
#' @details Future versions may include option to import csv files.
#'
#' @param input_path Path to the *.rds files to be assembled. Default is the Open
#'  Data/County Datasets folder on the CMAR Operations shared drive (user must
#'  be connected to the Perennia VPN).

#' @param county Vector of character string(s) indicating which county or
#'   counties for which to import data. For efficiency, the filter is applied to
#'   the file path, so the county name MUST be part of the file path (e.g., in
#'   the name of the file). Defaults to all counties.
#'
#' @param add_county_col Logical argument indicating whether to include a
#'   "COUNTY" column in the output. If \code{TRUE}, the imported data must have
#'   "WATERBODY" and "STATION" columns (which are used to join with the Area
#'   Info tab of the STRINGS TRACKING sheet so the COUNTY column can be added.
#'   Default is \code{TRUE}.
#' @family OpenData CMAR
#' @author Danielle Dempsey
#'
#' @importFrom purrr map_dfr
#' @importFrom data.table fwrite
#' @importFrom stringr str_subset
#' @importFrom googlesheets4 gs4_deauth read_sheet
#' @import dplyr
#' @export

import_strings_data <- function(input_path = NULL,
                                county = "ALL",
                                add_county_col = TRUE) {

  message("importing ", paste(county, collapse = " and "), " data...")

  # Input path --------------------------------------------------------------

  if(is.null(input_path)){
    # path to Open Data folder
    input_path <- "Y:/Coastal Monitoring Program/Open Data/County Datasets"

  } else input_path <- input_path

  # list rds files on the path and import -----------------------------------

  dat <- list.files(input_path, full.names = TRUE, pattern = ".rds")

  # filter for specified county(ies)
  # format county argument as a regular expression for use in str_subset
  if(!("ALL" %in% county)) dat <- dat %>% str_subset(paste(county, collapse = "|"))

  # read and bind the rds files
  dat <- dat %>%
    purrr::map_dfr(readRDS) %>%
    mutate(
      WATERBODY = case_when(
        WATERBODY == "St. Mary's Bay" ~ "St. Marys Bay",
        WATERBODY == "Larry's River" ~ "Larrys River",
        TRUE ~ WATERBODY
      ),
      STATION = case_when(
        STATION == "Sandy Cove St. Mary's" ~ "Sandy Cove St. Marys",
        STATION == "Larry's River" ~ "Larrys River",
        TRUE ~ STATION
      )
    )

  # merge with Area_Info table to add COUNTY column
  if(add_county_col == TRUE){

      if(!("WATERBODY" %in% colnames(dat) & "STATION" %in% colnames(dat))) {
      stop("Can't add COUNTY column because WATERBODY and/or STATION columns do not exist. \n
           HINT: Set add_county_col = FALSE, check input_path is correct, make sure you
           are connected to the VPN, or re-export files in input_path so they include the WATERBODY and STATION columns.")
    }

    # allow access to the google sheet
    googlesheets4::gs4_deauth()

    # link to the "STRING TRACKING" google sheet
    link <- "https://docs.google.com/spreadsheets/d/1a3QvJsvwr4dd64g3jxgewRtfutIpsKjT2yrMEAoxA3I/edit#gid=828367890"

    # read in the "Area Info" tab of the STRING TRACKING sheet
    Area_Info <- suppressMessages(googlesheets4::read_sheet(link, sheet = "Area Info")) %>%
      select(COUNTY = county, STATION = station, WATERBODY = waterbody) %>%
      mutate(STATION = as.character(STATION))

    # # check if there are any WATERBODYs or STATIONs in dat that are NOT in Area_Info
    # if(any(!(unique(dat$WATERBODY) %in% Area_Info$WATERBODY))){
    #
    #   waterbody_distinct <- dat %>%
    #     distinct(WATERBODY)  %>%
    #     filter(!(WATERBODY %in% Area_Info$WATERBODY))
    #
    #   warning("Found WATERBODY in county data that is not in Area_Info: ",
    #           waterbody_distinct$WATERBODY)
    # }

    # if(any(!(unique(dat$STATION) %in% Area_Info$STATION))){
    #
    #   station_distinct <- dat %>%
    #     distinct(STATION) %>%
    #     filter(!(STATION %in% Area_Info$STATION))
    #
    #   warning(
    #     "Found STATION in county data that is not in Area_Info: ",
    #     paste(station_distinct$STATION), collapse = "\n")
    # }

    # merge dat and Area_Info
    dat <- left_join(dat, Area_Info, by = c("WATERBODY", "STATION")) %>%
     # select(-Notes) %>%
      select(COUNTY, WATERBODY, STATION, everything()) %>%
      mutate(
        COUNTY = case_when(

          STATION == "Denys Basin East" ~ "Inverness",
          STATION == "Denys Basin West" ~ "Inverness",

          STATION == "Little Narrows - N" ~ "Victoria",
          STATION == "Little Narrows - S" ~ "Victoria",

          STATION == "Nyanza Bay - E" ~ "Victoria",
          STATION == "Nyanza Bay - W" ~ "Victoria",

          STATION == "Sandy Cove Chedabucto" ~ "Guysborough",
          STATION == "Sandy Cove St. Marys" ~ "Digby",

          STATION == "St. Andrews Channel - E" ~ "Cape Breton",
          STATION == "St. Peters Canal - N" ~ "Richmond",
          STATION == "St. Peters Canal - S" ~ "Richmond",

          TRUE ~ COUNTY
        )
      )

    # check if there are any stations with COUNTY = NA
    dat_check <- dat %>%
      filter(is.na(COUNTY)) %>%
      distinct(STATION)

    if(nrow(dat_check) > 0) {
      warning(
        "Found station(s) without COUNTY: ",
        paste(dat_check$STATION), collapse = ", ")

    }
  }

   dat
}



# if(FALSE){
#
#   library(dplyr)
#   library(stringr)
#   library(googlesheets4)
#
#   ALL <- import_strings_data()
#   ALL %>% distinct(COUNTY)
#
#   hali <- import_strings_data(add_county_col = FALSE, county = "Halifax")
#   hali %>% distinct(WATERBODY)
#
#   hali_lun <- import_strings_data(county = c("Halifax", "Lunenburg"))
#   hali_lun %>% distinct(COUNTY)
# }




