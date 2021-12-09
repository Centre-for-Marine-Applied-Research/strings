#' Read in csv file of county data assembled for Open Data Portal
#'
#' @description Reads in the file using data.table::fread and assigns the
#'   appropriate class to each column.
#'
#' @param path Path to the csv file, including file name and extension.
#'
#' @return Returns a dataframe of the assembled data.
#'
#' @importFrom data.table fread
#' @importFrom dplyr mutate if_else
#' @importFrom lubridate force_tz
#' @export

read_opendata_csv <- function(path){

  fread(
    path,
    colClasses = list(
      character = c("STATION", "LEASE", "DEPLOYMENT_PERIOD",
                    "SENSOR", "DEPTH", "VARIABLE", "UNITS", "MOORING"),
      POSIXct = "TIMESTAMP"),
    data.table = FALSE
  ) %>%
    mutate(
      LEASE = if_else(LEASE == "NA", NA_character_, LEASE),
      TIMESTAMP = force_tz(TIMESTAMP, tzone = "UTC")
    )

}




#' Read mooring table
#'
#' @param path Path to mooring table (including file name and extension).
#'
#' @return Mooring table with standardized WATERBODY and STATION columns.
#' @export

read_mooring_csv <- function(path){

  #path <- file.path("C:/Users/Danielle Dempsey/Desktop/MOORING_table.csv")

  dat_raw <- fread(path, data.table = FALSE)

  dat <- dat_raw %>%
    # fix spelling discrepancies
    mutate(
      WATERBODY = case_when(
        Waterbody == "Pipers lake" ~ "Piper Lake",
        Waterbody == "St Marys Bay" ~ "St. Mary's Bay",
        Waterbody == "St Margarets Bay" ~ "St. Margarets Bay",
        Waterbody == "St Anns Bay" ~ "St. Ann's Bay",
        Waterbody == "St Peters Inlet" ~ "St Peter's Inlet",
        Waterbody == "Owls Head Bay" ~ "Owl's Head Bay",
        TRUE ~ Waterbody
      ),
      STATION = case_when(
        Station_Name == "0191 / 1113" ~ "0191/1113",
        Station_Name == "Beaver point"  ~ "Beaver Point",
        Station_Name == "Big point pond"  ~ "Big Pond Point",
        Station_Name == "Burnt island"  ~ "Burnt Island",
        Station_Name == "Long beach" ~ "Long Beach",
        Station_Name == "Long Island two" ~ "Long Island 2",
        Station_Name == "Monks head" ~ "Monks Head",
        Station_Name == "Owls head" ~ "Owl's Head",
        Station_Name == "Pipers lake" ~ "Piper Lake",
        Station_Name ==  "Shut-in Island" ~ "Shut-In Island",
        Station_Name ==  "South side" | Station_Name == "South Side" ~ "Southside",
        Station_Name == "Tor Bay Center" ~ "Center Bay",
        TRUE ~ Station_Name
      )
    ) %>%
    select(-Waterbody, -Station_Name) %>%
    # CMAR corrected some Waterbodies and re-named some stations
    mutate(
      WATERBODY = case_when(
        WATERBODY == "Whycocomagh Bay" & STATION == "Deep Basin" ~ "Whycocomagh Basin",
        WATERBODY == "Antigonish Harbour" & STATION == "Captains Pond" ~ "Grahams Cove",
        WATERBODY == "Antigonish Harbour" & STATION == "Monks Head" ~ "Monks Head Harbour",
        WATERBODY == "Pictou Harbour" & STATION == "Melmerby Beach" ~ "Little Harbour",
        TRUE ~ WATERBODY
      ),
      STATION = case_when(
        WATERBODY == "Hourglass Lake" & STATION == "Deep Point" ~ "Hourglass Lake",
        WATERBODY == "Arichat Harbour" & STATION == "667" ~ "0667",
        WATERBODY == "Strait of Canso" & STATION == "Pulp Mill 2" ~ "Pulp Mill Site 2",
        WATERBODY == "St. Mary's Bay" & STATION == "Sandy Cove" ~ "Sandy Cove St. Mary's",
        WATERBODY == "Chedabucto Bay" & STATION == "Sandy Cove" ~ "Sandy Cove Chedabucto",
        TRUE ~ STATION
      )
    ) %>%
    mutate(MOORING = if_else(Surface.Float. == "Y", "float", "fixed")) %>%
    select(-Surface.Float.)

}









