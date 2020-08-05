#'@title Export sensor string data for the OpenData portal
#'@param path.data Path to deployment folder
#'@param path.export Path to where csv will be exported
#'@param deployment.name Name of deployment folder. Will be used as name of the csv file.
#'@param file.name Name of file to import. Must be a csv

#'@return Exports a csv file in the format for the OpenData portal.

#'@family OpenData
#'@author Danielle Dempsey
#'@importFrom readr read_csv write_csv

#'@export

export_opendata <- function(path.data, path.export, deployment.name, file.name) {

  # path to deployment folder
  path <- paste(path.data, deployment.name, sep = "/")

  # read deployment log and extract area info
  log <- read_deployment_log(path)
  location <- log$area.info

  # read in deployment data
  # separate(deployment.name, col = d3 , into = c("AREA", "DATE"), sep = "(?<=[a-zA-Z])\\s*(?=[0-9])")
  dat <- read_csv(paste(path, "/", file.name, ".csv", sep = ""))

  # add area info columns to deployment data
  dat_opendata <- format_for_opendata(dat, location)

  # export spreadsheet
  write_csv(dat_opendata, path = paste(path.export, "/", deployment.name, ".csv", sep = ""))


}
