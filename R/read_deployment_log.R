#'@title Extract information from deployment log
#'@details The log must be saved in .xlsx or .xls format, and must include the
#'  following columns:
#'
#'  \code{Deployment_Waterbody} (waterbody where string was deployed),
#'  \code{Location_Description} (the station name), \code{Deployment} (the
#'  deployment date, in the order "Ymd"), \code{Retrieval} (the retrieval
#'  date, in the order "Ymd"), \code{Logger_Latitude} (the latitude at which
#'  the string was deployed), \code{Logger_Longitude} (the longitude at which
#'  the string was deployed), \code{Logger_Model} (the logger type; see below
#'  for options), \code{Serial#} (the logger serial number), and
#'  \code{Sensor_Depth} (depth at which the sensor was deployed). All other
#'  columns will be ignored.
#'
#'  Entries in the \code{Logger_Model} column can be "HOBO Pro V2" (or "HOBO pro
#'  V2"), "aquaMeasure DOT", "aquaMeasure SAL", "aquaMeasure SST", or "VR2AR".
#'
#'  A warning will be printed to the console if there is more than one unique
#'  entry in \code{Deployment_Waterbody}, \code{Location_Description},
#'  \code{Deployment}, \code{Retrieval}, \code{Logger_Latitude}, or
#'  \code{Logger_Longitude}.
#'
#'@param path.log File path to the Log folder. T
#'@return Returns a list with 5 elements. \code{deployment.dates} is a dataframe
#'  with two columns: \code{start.date} (the date of deployment) and
#'  \code{end.date} (date of retrieval). area.info is a dataframe with four
#'  columns: waterbody, latitude, longitude, and station. HOBO, aM, and vemco
#'  are each a dataframe with two columns: SENSOR (serial number) and the
#'  corresponding DEPTH (depth of deployment in m).
#'@family compile
#'@author Danielle Dempsey
#'
#'@importFrom tidyr separate
#'@importFrom readxl read_excel
#'@importFrom lubridate ymd
#'@import dplyr
#'@export

read_deployment_log <- function(path.log){


# Read in log -----------------------------------------------------------

  path.log <- paste(path.log, "Log", sep = "/")

  dat.files <- list.files(path.log, all.files = FALSE, pattern = "*xlsx|*xls")

  # remove files that start with "~"
  if(any(substring(dat.files, 1, 1)== "~")) {

    dat.files <- dat.files[-which(substring(dat.files, 1, 1)== "~")]
    print(paste("Note:", sum((substring(dat.files, 1, 1)== "~")),
                "files on the path begin with ~ and were not imported.", sep = " "))
  }

  log <- read_excel(paste(path.log,  dat.files[1], sep = "/"))



# extract data ------------------------------------------------------------

  # deployment dates
  start  <- unique(log$Deployment)
  end <- unique(log$Retrieval)

  # warning if there is more than one Deployment or Retrieval date
  if(length(start) > 1 | length(end) > 1) print("WARNING: multiple Deployment or Retrieval dates in log")

  # Stop with ERROR if the dates are not in the proper format
  if(is.na(suppressWarnings(ymd(log$Deployment[1]))) |
     is.na(suppressWarnings(ymd(log$Retrieval[1])))) stop("Deployment and Retrieval dates must be in the order year, month, day")

  # deployment info to export
  deployment.dates <- data.frame(start.date = ymd(log$Deployment[1]),
                                 end.date = ymd(log$Retrieval[1]))

  # area info
  wb <- unique(log$Deployment_Waterbody)
  lat <- unique(log$Logger_Latitude)
  long <- unique(log$Logger_Longitude)
  station <- unique(log$Location_Description)

  if(length(wb) > 1) print("WARNING: multiple waterbodies in log")
  if(length(lat) > 1) print("WARNING: multiple latitudes recorded in log")
  if(length(long) > 1) print("WARNING: multiple longitudes recorded in log")
  if(length(station) > 1) print("WARNING: multiple location descriptions recorded in log")

  area.info <- data.frame(waterbody = log$Deployment_Waterbody[1],
                          latitude = log$Logger_Latitude[1],
                          longitude = log$Logger_Longitude[1],
                          station  = log$Location_Description[1])

  # HOBO sensors
  hobos <- log %>%
    filter(Logger_Model == "HOBO Pro V2" | Logger_Model == "HOBO pro V2" ) %>%
    select(Logger_Model, `Serial#`, Sensor_Depth) %>%
    separate(Logger_Model, into = c("sensor", NA, NA), sep = " ") %>%
    mutate(SENSOR = paste(sensor, `Serial#`, sep = "-"),
           DEPTH = paste(Sensor_Depth, "m", sep = "")) %>%
    select(SENSOR, DEPTH)

  # aquaMeasure sensors
  aquaMeasures <- log %>%
    filter(Logger_Model == "aquaMeasure DOT" | Logger_Model == "aquaMeasure SAL" |
             Logger_Model == "aquaMeasure SST") %>%
    select(Logger_Model, `Serial#`, Sensor_Depth) %>%
    separate(Logger_Model, into = c("sensor", NA), sep = " ") %>%
    mutate(SENSOR = paste(sensor, `Serial#`, sep = "-"),
           DEPTH = paste(Sensor_Depth, "m", sep = "")) %>%
    select(SENSOR, DEPTH)

  # vemco sensor
  vemcos <- log %>%
    filter(Logger_Model == "VR2AR") %>%
    select(Logger_Model, `Serial#`, Sensor_Depth) %>%
    mutate(SENSOR = paste(Logger_Model, `Serial#`, sep = "-"),
           DEPTH = paste(Sensor_Depth, "m", sep = "")) %>%
    select(SENSOR, DEPTH)

  # return list of deployment info
  list(deployment.dates = deployment.dates,
       area.info = area.info,
       HOBO = hobos,
       aM = aquaMeasures,
       vemco = vemcos)

}
