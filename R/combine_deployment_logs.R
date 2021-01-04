#'@title Combines logs from a station to make deployment history file
#'@details Imports the log from each deployment folder, binds them together, and
#'  re-formats into the deployment history file. Logs can be in .xls, .xlsx, or
#'  .csv format.
#'@param path Path to station folder.
#'@return Exports the deployment history file to the station folder, in .csv
#'  format.
#'@author Danielle Dempsey

#'@importFrom tidyr separate
#'@importFrom readxl read_excel
#'@importFrom readr read_csv write_csv
#'@importFrom dplyr select
#'@export

combine_deployment_logs <- function(path) {

  folders <- list.dirs(path, recursive = FALSE) # folders in path/
  LOG <- NULL

  # import the log file from each folder and bind together
  for(i in 1:length(folders)){

    d.i <- folders[i]                                    # path to deployment folder

    file.i <- list.files(paste(d.i, "Log", sep = "/"), pattern = "*xls|*xlsx|*csv")   # deployment log file name (including extension)

    # remove files that start with "~"
    if(any(substring(file.i, 1, 1) == "~")) {

      message(paste("Note:", sum((substring(file.i, 1, 1)== "~")),
                    "files on the path begin with ~ and were not imported.", sep = " "))
      file.i <- file.i[-which(substring(file.i, 1, 1)== "~")]

    }

    path.log.i <- paste(d.i, "Log", file.i, sep = "/")   # full path to the log, including file name

    # extract file extension
    extension <- file.i %>%
      data.frame() %>%
      separate(col = 1, into = c(NA, "EXT"), sep = "\\.")
    extension <- extension$EXT

    if(extension == "xls" | extension == "xlsx") log.i <- read_excel(path.log.i)

    if(extension == "csv") log.i <- read_csv(path.log.i)

    LOG <- rbind(LOG, log.i) # rbind all logs together
  }

  # format for Deployment History
  HIST <- LOG %>%
    dplyr::select(`Body of Water` = Deployment_Waterbody,
           `Location Description` = Location_Description,
           `Lease #` = `Lease#`,
           `Deployment Date` = Deployment,
           `Retrieval Date` = Retrieval,
           Duration = Duration,
           Latitude = Logger_Latitude,
           Longitude = Logger_Longitude,
           `Logger Model` = Logger_Model,
           `Serial Number` = `Serial#`,
           `Sensor Depth (m)` = Sensor_Depth,
           `Depth of Water (m)` = Sounding,
           `Acoustic Release` = `Acoustic_Release?`,
           Comments = Comments)

  hist.name <- paste(HIST$`Location Description`[1], "_Deployment_History.csv", sep = "")

  write_csv(HIST, paste(path, hist.name, sep = "/"))
}



