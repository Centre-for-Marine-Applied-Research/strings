#' Example deployment log
#' @format This example deployment log illustrates the columns required for use
#'   with the \code{read_deployment_log()} function. The deployment log must be saved
#'   in the Log folder, be named "station deployment-date Log" (e.g., Borgles
#'   Island 2018-02-28 Log), and be in .csv, .xlsx, or .xls format. There is one
#'   row for each sensor on the string, and 10 columns (other columns will be
#'   ignored by the package):  \describe{ \item{Deployment_Waterbody}{Waterbody
#'   where string was deployed.} \item{Location_Description}{The station name.}
#'   \item{Lease#}{If located on an aquaculture site, the lease number (NA
#'   otherwise).} \item{Deployment}{The deployment date, in the order "Ymd".}
#'   \item{Retrieval}{The retrieval date, in the order "Ymd".}
#'   \item{Logger_Latitude}{The latitude at which the string was deployed.}
#'   \item{Logger_Longitude}{The longitude at which the string was deployed.}
#'   \item{Logger_Model}{The type of sensor.} \item{Serial#}{The sensor serial
#'   number.} \item{Sensor_Depth}{The depth at which the sensor was deployed.}
#'
#'   }

#' @source Data from Coastal Monitoring Program
"deployment_log"



