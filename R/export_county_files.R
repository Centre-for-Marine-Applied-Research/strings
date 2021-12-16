#' @title Export data into a csv and/or rds file
#'
#' @param dat Dataframe to be exported, e.g., output from
#'   \code{export_county_data()}.
#'
#' @param file_name Name for the output file. Will be appended with today's
#'   date.

#' @param output_path Path to where assembled file(s) should be exported. The
#'   default will export the csv file to the Open Data/Submissions folder on the
#'   CMAR Operations shared drive (for submission to the Open Data Portal) and
#'   the rds file to the Open Data/County Datasets folder. For custom
#'   \code{output_path} entries, the csv and rds files will be exported to the
#'   same folder.
#'
#' @param export_csv Logical argument indicating whether the data should be
#'   exported as a *.csv file. File name will be county_todays-date.csv. Default
#'   is \code{TRUE}. Note: \code{TIMESTAMP} is converted to a character before
#'   exporting to remove UTC formatting (2018-12-23T05:00:00Z). When re-imported
#'   into R, the UTC timezone should be added using the \code{force_tz()}
#'   function from the lubridate package.
#'
#' @param export_rds Logical argument indicating whether the assembled data
#'   should be exported as a *.rds file. File name will be
#'   county_todays-date.rds. Default is \code{TRUE}.
#'
#' @param return_global Logical argument indicating whether the assembled data
#'   should be returned to the global environment. Default is \code{FALSE}.
#'
#' @return Exports \code{dat} as a csv and/or rds file.
#'
#' @family OpenData CMAR
#'
#' @author Danielle Dempsey
#'
#' @importFrom data.table fwrite
#' @importFrom dplyr mutate
#' @export

export_county_files <- function(dat,
                                file_name = "",
                                output_path = NULL,
                                export_csv = TRUE,
                                export_rds = TRUE,
                                return_global = FALSE) {

  # today's date (for file name)
  today_date <- Sys.Date()

  # Export csv --------------------------------------------------------------

  if(export_csv == TRUE){

    message("exporting csv for ",  file_name)

    # path and file name of output
    if(is.null(output_path)){

      # output file name
      output_csv <- paste("Y:/Coastal Monitoring Program/Open Data/Submissions",
                          paste0(file_name, "_", today_date, ".csv"), sep = "/")

    } else output_csv <- output_path

    dat %>%
      # remove the UTC formatting for Open Data Portal
      mutate(TIMESTAMP = format(TIMESTAMP)) %>%
      data.table::fwrite(file = output_csv, na = "NA", showProgress = TRUE)
  }

  # Export rds --------------------------------------------------------------

  if(export_rds == TRUE){

    message("exporting rds for ",  file_name)

    # path and file name of output
    if(is.null(output_path)){

      output_rds <- paste("Y:/Coastal Monitoring Program/Open Data/County Datasets",
                          paste0(file_name, "_", today_date, ".rds"), sep = "/")

    } else output_rds <- output_path

    saveRDS(dat, file = output_rds)

  }

  if(return_global == TRUE) dat

}
