#'@title Extract information from deployment log
#'@details The log must be saved in a folder called Log in .csv, .xlsx or .xls
#'  format, and must include the following columns:
#'
#'  \code{Deployment_Waterbody}: waterbody where string was deployed
#'
#'  \code{Location_Description}: the station name
#'
#'  \code{Lease#}: If located on an aquaculture site, the lease number (NA
#'  otherwise)
#'
#'  \code{Deployment}: The deployment date, in the order "Ymd"
#'
#'  \code{Retrieval}: The retrieval date, in the order "Ymd"
#'
#'  \code{Logger_Latitude}: The latitude at which the string was deployed
#'
#'  \code{Logger_Longitude} The longitude at which the string was deployed
#'
#'  \code{Logger_Model} The type of sensor; see below for options
#'
#'  \code{Serial#} The sensor serial number
#'
#'  \code{Sensor_Depth}: Depth at which the sensor was deployed
#'
#'  All other columns will be ignored.
#'
#'  Entries in the \code{Logger_Model} column can be "HOBO Pro V2", "HOBO DO",
#'  "TidbiT MX2303", "TidbiT MX2203"  "aquaMeasure DOT", "aquaMeasure SAL",
#'  "aquaMeasure SST", or "VR2AR". (Some mis-spellings are accepted: "HOBO pro
#'  V2", "HOBO_Pro_V2", "aquameasure DOT", "aquameasure SAL", "aquameasure SST")
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
#'  If there is more than one eligible file (csv, .xlsx or .xls) in the Log
#'  folder, the function will stop with an error.
#'
#' @param path.log File path to the Log folder.
#' @return Returns a list with 5 elements. \code{deployment.dates} is a dataframe
#'  with two columns: \code{start.date} (the date of deployment) and
#'  \code{end.date} (date of retrieval). \code{area.info} is a dataframe with
#'  five columns: \code{waterbody}, \code{latitude}, \code{longitude},
#'  \code{station}, and \code{lease}. \code{HOBO}, \code{aM}, and \code{vemco}
#'  are each a dataframe with two columns: \code{SENSOR} (serial number) and the
#'  corresponding \code{DEPTH} (depth of deployment in m).
#' @family compile
#' @author Danielle Dempsey
#'
#' @importFrom tidyr separate
#' @importFrom stringr str_replace str_detect
#' @importFrom readxl read_excel
#' @importFrom readr read_csv
#' @importFrom lubridate ymd
#' @import dplyr
#' @export

#' @examples
#' # path to "Log" folder
#' path <- system.file("extdata", package = "strings")
#'
#' log <- read_deployment_log(path)
#' # deployment and retrieval dates
#' dates <- log$deployment.dates
#' # deployment location information
#' location <- log$area.info
#' # serial number(s) and depth(s) of deployment for Hobo and TidiT sensors on the string
#' serial.table.HOBO <- log$HOBO
#' # serial number(s) and depth(s) of deployment for aquaMeasure sensors on the string
#' serial.table.aM <- log$aM
#' # depth of the vemco sensor
#' depth.vemco <- log$vemco

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

  if(length(dat.files) > 1) stop("More than one file found in the Log folder")

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

  if(long > 0) stop("Longitude must be a negative value")

  area.info <- data.frame(waterbody = log$Deployment_Waterbody[1],
                          latitude = log$Logger_Latitude[1],
                          longitude = log$Logger_Longitude[1],
                          station  = log$Location_Description[1],
                          lease = log$`Lease#`[1])


  sensors <- log %>%
    mutate(Logger_Model = str_replace(Logger_Model, "_", " "),
           Logger_Lower = tolower(Logger_Model),
           detect_hobo = str_detect(Logger_Lower, "hobo"),
           detect_tidbit = str_detect(Logger_Lower, "tidbit"),
           detect_am = str_detect(Logger_Lower, "aquameasure"),
           detect_vemco = str_detect(Logger_Lower, "vr2ar"))

  # print a warning if there are any sensors in the log that are NOT recognized by this function
  n_sensors <- sensors %>%
    select(contains("detect")) %>%
    apply(1, sum)

  if(any(n_sensors == 0)) {

    extra.sensor <- sensors[which(n_sensors == 0), "Logger_Model"]

    warning(extra.sensor, " was found in the Logger_Model column of the log.
            \nThis sensor is not recognized by the read_deployment_log() function")
  }

  # HOBO & TidBit sensors
  # hobo.sensors <- c("HOBO Pro V2", "HOBO pro V2", "HOBO_Pro_V2", "HOBO_PRO_V2",
  #                   "HOBO DO",
  #                   "TidbiT MX2303", "TidbiT MX2203")

  hobos <- sensors %>%
    # mutate(Logger_Model = str_replace(Logger_Model, "_", " "),
    #        Logger_Lower = tolower(Logger_Model),
    #        detect_hobo = str_detect(Logger_Lower, "hobo"),
    #        detect_tidbit = str_detect(Logger_Lower, "tidbit")) %>%
    filter(detect_hobo | detect_tidbit) %>%
    # filter(Logger_Model %in% hobo.sensors) %>%
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
  # aM.sensors <- c("aquaMeasure DOT", "aquaMeasure_DOT", "aquaMeasure SAL",
  #                 "aquaMeasure SST",
  #                 "aquameasure DOT", "aquameasure SAL", "aquameasure SST")

  aquaMeasures <- sensors %>%
    # mutate(Logger_Model = str_replace(Logger_Model, "_", " "),
    #        Logger_Lower = tolower(Logger_Model),
    #        detect = str_detect(Logger_Lower, "aquameasure")) %>%
    filter(detect_am) %>%
    #filter(Logger_Model %in% aM.sensors) %>%
    select(Logger_Model, `Serial#`, Sensor_Depth) %>%
    mutate(Logger_Model = str_replace(Logger_Model, "_", " ")) %>%
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
  #vemco.sensors <- "VR2AR"

  vemcos <- sensors %>%
    filter(detect_vemco) %>%
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


  # return list of deployment info
  list(deployment.dates = deployment.dates,
       area.info = area.info,
       HOBO = hobos,
       aM = aquaMeasures,
       vemco = vemcos)

}
