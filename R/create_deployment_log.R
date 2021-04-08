#' @title Writes deployment log from the NSDFA tracking sheet
#' @details Imports the NSDFA tracking sheet, filters for the station and date of
#'  interest, re-formats into the deployment log format, and exports to the Log
#'  folder.
#'
#'  DD: I tried to match Recv_Method from the tracking sheet with Mount_Type in
#'  the Log, but it doesn't match existing logs.
#' @param path.tracking.sheet Full path to the NSDFA tracking sheet (including
#'  the file name and extension). This must be a .xlsx file.
#' @param temp.sheet Name of the tab with the temperature data (in "").
#' @param path.export Path to the Log folder.
#' @param station Station name.
#' @param deployment.date Date of deployment as a character string in the format
#'  "Y-m-d"
#' @return Returns deployment log in .xlsx format.
#' @author Danielle Dempsey
#' @importFrom readxl read_excel
#' @importFrom openxlsx saveWorkbook
#' @importFrom lubridate as_date
#' @importFrom magrittr %T>%
#' @importFrom stringr str_to_title
#' @import dplyr
#' @export


create_log_from_NSDFA_tracking <- function(path.tracking.sheet,
                                           temp.sheet = "TempMetaData",
                                           path.export,
                                           station,
                                           deployment.date){

  dat_raw <- read_excel(path.tracking.sheet,
                        sheet = temp.sheet,
                        na = c("", "n/a"))

  LOG <- dat_raw %>%
    transmute(
      Deployment_Waterbody = Waterbody,
      Location_Description = Station_Name,
      `Lease#`,
      Status,

      Deployment = Depl_Date,
      Retrieval =  Recv_Date,
      Duration = Depl_Duration,
      Logger_Latitude = Depl_Lat,
      Logger_Longitude = Depl_Lon,
      Logger_Model = Inst_Model,
      `Serial#` = Inst_Serial,
      Sensor_Depth = Inst_Depth,

      Sounding = Depl_Sounding,
      Datum = NA,
      Mount_type = NA,

      `Acoustic_Release?` = ifelse(Recv_Method == "Acoustic_Release", "Y", NA_character_),
      `Surface_Buoy?` = ifelse(Recv_Method == "Surface Buoy", "Y", NA_character_),

      Deployment_Attendant = Depl_Attendant,
      Retrieval_Attendant = Recv_Attendant,

      Comments = Notes,

      `Deployment Waypoint` = NA,
      `Retrieval Waypoint` = NA,

      `Retrieval Latitude`	= Recv_Lat,
      `Retrieval Longitude` =	Recv_Lon,

      `Sensor Voltage deployed` = Depl_Voltage,
      `Sensor Voltage retrieved` = Recv_Voltage,

      `Vessel sounder offset + transponder depth` = NA,
      `verified measurement (below origin, first sensor under float)` = NA,
      `tide correction` = NA,
      `Rising or Falling` = NA,
      `height of VR2AR base off bottom` = NA,

      `time of deployment` = Depl_Time,
      `photos taken?` = NA,

      `Anchor type` = Anchor_Wgt,
      `Float type` = NA,
      `distance from top of float to origin (first sensor)` = NA
    ) %>%
    # match all lower case letters in case typos
    mutate(Location_lower = tolower(Location_Description))


  if(!(tolower(station) %in% unique(LOG$Location_lower))) {

    warning(paste0(station, " not found in NSDFA Tracking sheet"))
  }


# Finishing touches -------------------------------------------------------
  LOG <- LOG %>%
    dplyr::filter(
      Location_lower == tolower(station) &
        Deployment == as_date(deployment.date)
    ) %>%
    dplyr::mutate(
      Location_Description = str_to_title(Location_Description),
      `Serial#` = as.numeric(`Serial#`),
      Sounding = as.numeric(Sounding),
      Deployment = format(as_date(Deployment), "%Y-%b-%d"),
      Retrieval = format(as_date(Retrieval), "%Y-%b-%d")
    ) %>%
    select(-Location_lower)

  if(suppressWarnings(is.numeric(as.numeric(LOG$Sensor_Depth)))){
    LOG <- LOG %>% mutate(Sensor_Depth = as.numeric(Sensor_Depth))
  }

  # Format & Export ---------------------------------------------------------

  file.name <- paste(station, format(as_date(deployment.date), "%Y-%m-%d"),
                     "Log.xlsx", sep = " ")

  # write_csv(LOG, paste(path.export, file.name, sep = "/"))

  LOG_OUT <- LOG %>%
    format_deployment_log() %T>%
    saveWorkbook(file = paste(path.export, file.name, sep = "/"))

  message(file.name, " exported to ", path.export)
}



#' @title Exports an initial deployment log
#' @details Deployment_Waterbody and Location_Description are filled in.
#'   Remaining columns are empty.
#' @param station Station name (for the Location_Description column).
#' @param waterbody Waterbody name (for the Deployment_Waterbody column). If
#'   \code{waterbody} == "lookup" (the default), the function will look up the
#'   waterbody corresponding to \code{station} in the Area Info tab of the
#'   STRINGS TRACKING google spreadsheet.
#' @param path.export Path to where the log should be exported.
#' @param n.sensors Number of sensors that will be included in the string, if
#'   known. Default is \code{n.sensors = 5}.
#' @return Returns deployment log in .xlsx format.
#' @author Danielle Dempsey

#' @importFrom openxlsx saveWorkbook
#' @importFrom lubridate as_date
#' @importFrom magrittr %T>%
#' @importFrom googlesheets4 gs4_deauth
#' @importFrom googlesheets4 read_sheet
#' @import dplyr
#' @export

create_initial_deployment_log <- function(station,
                                          waterbody = "lookup",
                                          path.export,
                                          n.sensors = 5){

  file.name <- paste(station, "XXXX-XX-XX Log.xlsx", sep = " ")

  if(waterbody == "lookup"){

    # allow access to the google sheet
    googlesheets4::gs4_deauth()

    # link to the "STRING TRACKING" google sheet
    link <- "https://docs.google.com/spreadsheets/d/1a3QvJsvwr4dd64g3jxgewRtfutIpsKjT2yrMEAoxA3I/edit#gid=828367890"

    # read in the "Area Info" tab of the STRING TRACKING sheet
    Area_Info <- googlesheets4::read_sheet(link, sheet = "Area Info")

    # look up the Station name in the Area Info tab and return the Waterbody
    waterbody <- Area_Info[which(Area_Info$Station == station), "Waterbody"]
    waterbody <- waterbody$Waterbody

    if(length(waterbody) == 0){

      stop(
        paste("\nCannot find", station, "in the STRINGS TRACKING google doc. \n
      HINT: check spelling in function and in google doc
              OR define waterbody in the function call.")
      )
    }
  }

  LOG <- tibble(Deployment_Waterbody = rep(waterbody, times = n.sensors),
                Location_Description = rep(station, times = n.sensors)) %>%
    mutate(
      `Lease#` = NA,
      Status = NA,

      Deployment = NA,
      Retrieval = NA,
      Duration = NA,
      Logger_Latitude = NA,
      Logger_Longitude = NA,
      Logger_Model = NA,
      `Serial#` = NA,
      Sensor_Depth = NA,

      Sounding = NA,
      Datum = NA,
      Mount_type = NA,

      `Acoustic_Release?` = NA,
      `Surface_Buoy?` = NA,

      Deployment_Attendant = NA,
      Retrieval_Attendant = NA,

      Comments = NA,

      `Deployment Waypoint` = NA,
      `Retrieval Waypoint` = NA,

      `Retrieval Latitude`	 = NA,
      `Retrieval Longitude`  = NA,

      `Sensor Voltage deployed` = NA,
      `Sensor Voltage retrieved` = NA,

      `Vessel sounder offset + transponder depth` = NA,
      `verified measurement (below origin, first sensor under float)` = NA,
      `tide correction` = NA,
      `Rising or Falling` = NA,
      `height of VR2AR base off bottom` = NA,

      `time of deployment` = NA,
      `photos taken?` = NA,

      `Anchor type` = NA,
      `Float type` = NA,
      `distance from top of float to origin (first sensor)` = NA
    ) %>%
    format_deployment_log() %T>%
    saveWorkbook(file = paste(path.export, file.name, sep = "/"))

  message(file.name, " exported to ", path.export)
}

