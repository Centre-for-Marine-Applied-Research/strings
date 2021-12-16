#' @title Assemble sensor string data from different deployments
#'
#' @description Only users connected to the CMAR Operations shared drive can use
#'   the default \code{input_path}.
#'
#' @details Right now can only handle files exported from
#'   \code{format_for_opendata()}. Future versions will also be able to assemble
#'   data from \code{convert_to_tidydata()}.
#'
#' @param input_path Path to the *.csv files to be assembled. Default is the
#'   Open Data/Deployment Data folder on the CMAR Operations shared drive (user
#'   must be connected to the Perennia VPN).
#'
#' @param county A character string. If \code{input_path = NULL}, \code{county}
#'   is the next folder on the path.
#'
#' @return Returns a dataframe of the assembled county data.
#'
#' @family OpenData CMAR
#'
#' @author Danielle Dempsey
#'
#' @importFrom purrr map_dfr
#' @importFrom data.table fread
#' @importFrom dplyr if_else mutate
#' @export

assemble_county_data <- function(input_path = NULL, county = "") {

  message("importing data for ", county)

  # Input path --------------------------------------------------------------

  if(is.null(input_path)){
    # path to Open Data folder
    input_path <- paste0(
      "Y:/Coastal Monitoring Program/Open Data/Deployment Data/", county
    )

  }

  # list csv files on the path and import -----------------------------------
  list.files(input_path, full.names = TRUE, pattern = ".csv") %>%
      purrr::map_dfr(
        data.table::fread,
        colClasses = list(
          character = c("STATION", "LEASE", "DEPLOYMENT_PERIOD",
                        "SENSOR", "DEPTH", "VARIABLE"),
          POSIXct = "TIMESTAMP"),
        data.table = FALSE
      ) %>%
      mutate(
        LEASE = if_else(LEASE == "NA" | LEASE == "", NA_character_, LEASE),
        # to explicitly assign TIMESTAMP the UTC timezone
        TIMESTAMP = force_tz(TIMESTAMP, tzone = "UTC"),

        UNITS = case_when(
          VARIABLE == "Temperature" ~ "degrees Celsius",
          VARIABLE == "Salinity" ~ "practical salinity units",

          VARIABLE == "Dissolved Oxygen" & str_detect(SENSOR, "aquaMeasure") ~
            "percent saturation",
          VARIABLE == "Dissolved Oxygen" & str_detect(SENSOR, "HOBO") ~ "mg/L"
        )
      )
}
