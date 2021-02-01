#'@title Assemble sensor string data from different deployments into a csv
#'  and/or rds file
#'@description Only users connected to the CMAR Operations shared drive can use
#'  the default \code{input_path} and \code{output_path}.The csv and rds files
#'  are exported to different folders on the shared drive.
#'@details
#'
#'
#'
#'
#'This function gives the option to export as a csv file and/or an rds
#'  file. For the CMAR work flow, the csv file is for submission to the Open
#'  Data Portal and the rds file should be used for the county report and other
#'  R analyses (it is much faster to import the rds file than the csv file).
#'
#'  Note: data.table::fread and data.table::fwrite were investigated as a faster
#'  alternative to reading and writing rds files. The data.table functions were
#'  faster for working with files on the Desktop, but were slower for working
#'  from the shared drive. This was tested by reading in two csv files (293,318
#'  KB and 72,408 KB) and the same data in two rds files (9,620 KB and 2,363) 5
#'  times. data.table::fread took an average of 78 seconds to read in the two
#'  files, while readRDS() took an average of 9.9 seconds (~8 times faster).
#'  (readr::read_csv took an average of 105 seconds).
#'
#'  50,000 rows of this dataset were exported 5 times using data.table::fwrite
#'  and saveRDS. data.table::fwrite took an average of 12.9 seconds, while
#'  saveRDS took an average of 10.5 seconds (1.2 times faster).
#'  (readr::write_csv took an average of 350 seconds).
#'@param input_path Path to the *.csv files to be assembled. Default is the Open
#'  Data/Deployment Data folder on the CMAR Operations shared drive (user must
#'  be connected to the Perennia VPN).
#'@param output_path Path to where assembled file(s) should be exported. The
#'  default will export the csv file to the Open Data/Submissions folder on the
#'  CMAR Operations shared drive (for submission to the Open Data Portal) and
#'  the rds file to the Open Data/County Datasets folder (for import ). For
#'  custom \code{out_path} entries, the csv and rds files will be exported to
#'  the same folder.
#'@param county A character string. If \code{export_csv} and/or
#'  \code{export_rds} is \code{TRUE}, \code{county} is used to name the exported
#'  file(s). Additionally, if \code{input_path = NULL}, \code{county} is the
#'  next folder on the path.
#'@param submission_folder A character string to finish the default
#'  \code{input_path} (e.g., the date the folder with the deployment csv files).
#'  For non-CMAR users (or any non-Open Data Portal data query), the default
#'  value \code{submission_folder = ""} value should be used.
#'@param export_csv Logical argument indicating whether the assembled data
#'  should be exported as a *.csv file. File name will be
#'  county_todays-date.csv. Default is \code{TRUE}.
#'@param export_rds Logical argument indicating whether the assembled data
#'  should be exported as a *.rds file. File name will be
#'  county_todays-date.rds. Default is \code{TRUE}.
#'@param return_global Logical argument indicating whether the assembled data
#'  should be returned to the global environment. Default is \code{FALSE}.
#'@return Returns the assembled data as a .csv, and/or .rds, and/or as a
#'  dataframe to the global environment.
#'@family OpenData
#'@author Danielle Dempsey
#'
#'@importFrom purrr map_dfr
#'@importFrom data.table fread fwrite
#'@import dplyr
#'@export

assemble_opendata_submission <- function(input_path = NULL,
                                         output_path = NULL,
                                         county = "", submission_folder = "",
                                         export_csv = TRUE,
                                         export_rds = TRUE,
                                         return_global = FALSE) {

  message("importing data for ", county)

  # Input path --------------------------------------------------------------

  if(is.null(input_path)){
    # path to Open Data folder
    input_path <- paste("Y:/Coastal Monitoring Program/Open Data/Deployment Data",
                        county, submission_folder, sep = "/")

  } else input_path <- input_path


  # list csv files on the path and import -----------------------------------

  dat <- list.files(input_path,
                    full.names = TRUE,
                    pattern = ".csv") %>%
    purrr::map_dfr(data.table::fread,
                   colClasses = list(
                     character = c("DEPLOYMENT_PERIOD", "SENSOR", "DEPTH", "VARIABLE"),
                     POSIXct = "TIMESTAMP"
                   ))

  if(exists("dat$LEASE")) dat <- dat %>%  mutate(LEASE = if_else(LEASE == "NA", NA_character_, LEASE))

# Export csv --------------------------------------------------------------

  if(export_csv == TRUE){

    message("exporting csv for ",  county)
    # today's date (for file name)
    today_date <- Sys.Date()

    # path and file name of output
    if(is.null(output_path)){

      output_csv <- paste("Y:/Coastal Monitoring Program/Open Data/Submissions",
                           paste0(county, "_", today_date, ".csv"),                   # output file name
                           sep = "/")
    } else output_csv <- output_path

    data.table::fwrite(dat, file = output_csv, showProgress = TRUE)
  }


# Export rds --------------------------------------------------------------

  if(export_rds == TRUE){

    message("exporting rds for ",  county)
    # today's date (for file name)
    today_date <- Sys.Date()

    # path and file name of output
    if(is.null(output_path)){

      output_rds <- paste("Y:/Coastal Monitoring Program/Open Data/County Datasets",
                           paste0(county, "_", today_date, ".rds"),                   # output file name
                           sep = "/")
    } else output_rds <- output_path

    saveRDS(dat, file = output_rds)
  }


# Return to global environment --------------------------------------------

  if(return_global == TRUE) dat
}
