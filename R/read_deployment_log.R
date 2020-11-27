#'@title Extract information from deployment log
#'@details The log must be saved in .csv, .xlsx or .xls format, and must include
#'  the following columns:
#'
#'  \code{Deployment_Waterbody} (waterbody where string was deployed),
#'  \code{Location_Description} (the station name), \code{Lease#} If located on
#'  an aquaculture site, the lease number (NA otherwise), \code{Deployment} (the
#'  deployment date, in the order "Ymd"), \code{Retrieval} (the retrieval date,
#'  in the order "Ymd"), \code{Logger_Latitude} (the latitude at which the
#'  string was deployed), \code{Logger_Longitude} (the longitude at which the
#'  string was deployed), \code{Logger_Model} (the type of sensor; see below for
#'  options), \code{Serial#} (the sensor serial number), and \code{Sensor_Depth}
#'  (depth at which the sensor was deployed). All other columns will be ignored.
#'
#'  Entries in the \code{Logger_Model} column can be "HOBO Pro V2", "TidbiT
#'  MX2303", "TidbiT MX2203"  "aquaMeasure DOT", "aquaMeasure SAL", "aquaMeasure
#'  SST", or "VR2AR". (Some mis-spellings are accepted: ("HOBO pro V2",
#'  "HOBO_Pro_V2", "aquameasure DOT", "aquameasure SAL", "aquameasure SST")
#'
#'  A Warning message is printed to the console when the function does not
#'  recognize a sensor in the log.
#'
#'  A message is printed to the console when Hobo, aquaMeasure, or Vemco sensors
#'  are not found in the log.
#'
#'  A message will be printed to the console if there is more than one unique
#'  entry in \code{Deployment_Waterbody}, \code{Location_Description},
#'  \code{Deployment}, \code{Retrieval}, \code{Logger_Latitude}, or
#'  \code{Logger_Longitude}.
#'
#'@param path.log File path to the Log folder.
#'@return Returns a list with 5 elements. \code{deployment.dates} is a dataframe
#'  with two columns: \code{start.date} (the date of deployment) and
#'  \code{end.date} (date of retrieval). area.info is a dataframe with five
#'  columns: waterbody, latitude, longitude, station, and lease. HOBO, aM, and
#'  vemco are each a dataframe with two columns: SENSOR (serial number) and the
#'  corresponding DEPTH (depth of deployment in m).
#'@family compile
#'@author Danielle Dempsey
#'
#'@importFrom tidyr separate
#'@importFrom readxl read_excel
#'@importFrom readr read_csv
#'@importFrom lubridate ymd
#'@import dplyr
#'@export

read_deployment_log <- function(path.log){


# Read in log -----------------------------------------------------------

  path.log <- paste(path.log, "Log", sep = "/")

  dat.files <- list.files(path.log, all.files = FALSE, pattern = "*xlsx|*xls|*csv")

  # remove files that start with "~"
  if(any(substring(dat.files, 1, 1) == "~")) {

    message(paste("Note:", sum((substring(dat.files, 1, 1)== "~")),
                "files on the path begin with ~ and were not imported.", sep = " "))
    dat.files <- dat.files[-which(substring(dat.files, 1, 1)== "~")]

  }

  # extract file extension
  # extension <- dat.files %>%
  #   data.frame() %>%
  #   separate(col = 1, into = c(NA, "EXT"), sep = "\\.")
  # extension <- extension$EXT

  file.type <- extract_file_extension(dat.files)

  if(file.type == "xls" |  file.type == "xlsx") log <- read_excel(paste(path.log,  dat.files[1], sep = "/"))

  if(file.type == "csv") log <- read_csv(paste(path.log,  dat.files[1], sep = "/"))


  # extract data ------------------------------------------------------------

  # deployment dates
  start  <- unique(log$Deployment)
  end <- unique(log$Retrieval)

  # message if there is more than one Deployment or Retrieval date
  if(length(start) > 1 | length(end) > 1) message("Multiple Deployment or Retrieval dates in log")

  # Stop with ERROR if the dates are not in the proper format
  if(is.na(suppressWarnings(lubridate::ymd(log$Deployment[1]))) |
     is.na(suppressWarnings(lubridate::ymd(log$Retrieval[1])))) stop("Deployment and Retrieval dates must be in the order year, month, day")

  # deployment info to export
  deployment.dates <- data.frame(start.date = lubridate::ymd(log$Deployment[1]),
                                 end.date = lubridate::ymd(log$Retrieval[1]))

  # area info
  wb <- unique(log$Deployment_Waterbody)
  lat <- unique(log$Logger_Latitude)
  long <- unique(log$Logger_Longitude)
  station <- unique(log$Location_Description)
  lease <- unique(log$`Lease#`)

  if(length(wb) > 1) message("Multiple waterbodies in log")
  if(length(lat) > 1) message("Multiple latitudes recorded in log")
  if(length(long) > 1) message("Multiple longitudes recorded in log")
  if(length(station) > 1) message("Multiple location descriptions recorded in log")
  if(length(lease) > 1) message("Multiple leases recorded in log")

  area.info <- data.frame(waterbody = log$Deployment_Waterbody[1],
                          latitude = log$Logger_Latitude[1],
                          longitude = log$Logger_Longitude[1],
                          station  = log$Location_Description[1],
                          lease = log$`Lease#`[1])

  # HOBO & TidBit sensors
  hobo.sensors <- c("HOBO Pro V2", "HOBO pro V2", "HOBO_Pro_V2", "TidbiT MX2303", "TidbiT MX2203")

  hobos <- log %>%
    filter(Logger_Model %in% hobo.sensors) %>%
    select(Logger_Model, `Serial#`, Sensor_Depth) %>%
    separate(Logger_Model, into = c("sensor", NA, NA), sep = " ", fill = "right") %>%
    mutate(SENSOR = paste(sensor, `Serial#`, sep = "-"),
           numeric_depth = if_else(is.numeric(Sensor_Depth), TRUE, FALSE)) %>%
    # if depth is numeric, add "m" for meters
    mutate(DEPTH = if_else(numeric_depth == TRUE,
                                 paste(Sensor_Depth, "m", sep = ""), as.character(Sensor_Depth))) %>%
    select(SENSOR, DEPTH)

  if(nrow(hobos) == 0){
      hobos <- NULL
      message("No Hobo sensors found in log")
  }

  # aquaMeasure sensors
  aM.sensors <- c("aquaMeasure DOT", "aquaMeasure SAL", "aquaMeasure SST",
                  "aquameasure DOT", "aquameasure SAL", "aquameasure SST")

  aquaMeasures <- log %>%
    filter(Logger_Model %in% aM.sensors) %>%
    select(Logger_Model, `Serial#`, Sensor_Depth) %>%
    separate(Logger_Model, into = c("sensor", NA), sep = " ") %>%
    mutate(SENSOR = paste(sensor, `Serial#`, sep = "-"),
           numeric_depth = if_else(is.numeric(Sensor_Depth), TRUE, FALSE)) %>%
    # if depth is numeric, add "m" for meters
    mutate(DEPTH = if_else(numeric_depth == TRUE,
                           paste(Sensor_Depth, "m", sep = ""), as.character(Sensor_Depth))) %>%
    select(SENSOR, DEPTH)

  if(nrow(aquaMeasures) == 0){
    aquaMeasures <- NULL
    message("No aquaMeasure sensors found in log")
  }

  # vemco sensor
  vemco.sensors <- "VR2AR"

  vemcos <- log %>%
    filter(Logger_Model %in% vemco.sensors) %>%
    select(Logger_Model, `Serial#`, Sensor_Depth) %>%
    mutate(SENSOR = paste(Logger_Model, `Serial#`, sep = "-"),
           numeric_depth = if_else(is.numeric(Sensor_Depth), TRUE, FALSE)) %>%
    # if depth is numeric, add "m" for meters
    mutate(DEPTH = if_else(numeric_depth == TRUE,
                           paste(Sensor_Depth, "m", sep = ""), as.character(Sensor_Depth))) %>%
    select(SENSOR, DEPTH)

  if(nrow(vemcos) == 0){
    vemcos <- NULL
    message("No Vemco sensors found in log")
  }

  # print a warning if there are any sensors in the log that are NOT recognized by this function
  log.sensors <- unique(log$Logger_Model)
  script.sensors <- c(hobo.sensors, aM.sensors, vemco.sensors)

  if(any(!(log.sensors %in% script.sensors))) {

    extra.sensor <- log.sensors[which(!(log.sensors %in% script.sensors))]

    warning(extra.sensor, " was found in the Logger_Model column of the log.
            This sensor is not recognized by the read_deployment_log() function")
  }

  # return list of deployment info
  list(deployment.dates = deployment.dates,
       area.info = area.info,
       HOBO = hobos,
       aM = aquaMeasures,
       vemco = vemcos)

}
