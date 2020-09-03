#'@title Writes deployment log from the NSDFA tracking sheet
#'@details I tried to match Recv_Method from the tracking sheet with Mount_Type
#'  in the Log, but it doesn't match existing logs.
#'@param path.tracking.sheet Path to where "2020-08-10 - NSDFA_Tracking.xlsx" is
#'  saved.
#'@param path.export Path to the Log folder.
#'@param station Station name
#'@param deployment.date Date of deployment as a character string in the format
#'  "\%Y-\%m-\%d"
#'@return Fill this out
#'@author Danielle Dempsey
#'@importFrom readxl read_excel
#'@importFrom readr write_csv
#'@importFrom lubridate as_date
#'@import dplyr
#'@export


create_deployment_log <- function(path.tracking.sheet,
                                  path.export,
                                  station,
                                  deployment.date){


  dat_raw <- read_excel(paste(path.tracking.sheet, "2020-08-10 - NSDFA_Tracking.xlsx", sep = "/"),
                        sheet = "TempMetaData")

  LOG <- dat_raw %>%
    transmute(Deployment_Waterbody = Waterbody,
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

              `Acoustic_Release?` = ifelse(Recv_Method == "Acoustic_Release", "Y", "NA"),
              `Surface_Buoy?` = ifelse(Recv_Method == "Surface Buoy", "Y", "N"),

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
    dplyr::filter(Location_Description == station & Deployment == as_date(deployment.date)) %>%
    dplyr::mutate(Deployment = format(as_date(Deployment), "%Y-%b-%d"),
           Retrieval = format(as_date(Retrieval), "%Y-%b-%d"))


  file.name <- paste(station, format(as_date(deployment.date), "%Y-%m-%d"), "Log.csv", sep = " ")

  write_csv(LOG, paste(path.export, file.name, sep = "/"))

}

