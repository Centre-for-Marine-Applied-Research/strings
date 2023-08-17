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
#' @param path_mooring Path to mooring type table (including file name and
#'   extension).
#'
#' @return Mooring table with standardized WATERBODY and STATION columns.
#'
#' @importFrom data.table fread
#' @importFrom dplyr distinct case_when if_else mutate select
#' @importFrom lubridate dmy
#'
#' @export

read_mooring_csv <- function(path_mooring){

  #path <- file.path("Y:/Coastal Monitoring Program/Open Data/Deployment Data/MOORING_table.csv")

  dat_raw <- fread(path_mooring, data.table = FALSE, header = TRUE)

  dat <- dat_raw %>%
    # fix spelling discrepancies
    mutate(
      Depl_Date = dmy(Depl_Date),
      Recv_Date = dmy(Recv_Date),

      WATERBODY = case_when(
        Waterbody == "Black Harbour" &
          Station_Name == "Burnt Island" ~ "Merigomish Harbour",
        Waterbody == "Merigomish Harbour" &
          Station_Name == "Robinson Cove" ~ "French Channel",
         Waterbody == "Pipers lake" ~ "Piper Lake",
        Waterbody == "St Marys Bay" ~ "St. Mary's Bay",
        Waterbody == "St Margarets Bay" ~ "St. Margarets Bay",
        Waterbody == "St Anns Bay" ~ "St. Ann's Bay",
        Waterbody == "St Peters Inlet" ~ "St. Peters Inlet",

       # Waterbody == "Owls Head Bay" ~ "Owl's Head Bay",
       TRUE ~ Waterbody
      ),
      STATION = case_when(
        Station_Name == "1" ~ "0001",
        Station_Name == "28" ~ "0028",
        Station_Name == "75" ~ "Wine Harbour",
        Station_Name == "622" ~ "0622",
        Station_Name == "623" ~ "West Ferry Ramp",
        Station_Name == "745" ~ "0745",
        Station_Name == "778" ~ "0778",
        Station_Name == "994" ~ "0994",
        Station_Name == "1006" ~ "Saddle Island",

        # Station_Name == "0191 / 1113" ~ "0191/1113",
        Station_Name == "0191 / 1113" |  Station_Name == "0191/1113" ~ "Burnt Island",
        Station_Name == "1198" ~ "Northwest Branch",
        Station_Name == "1114" ~ "The Channel",
        Station_Name == "613" ~ "White Island",
        Station_Name == "1091" ~ "Yankee Cove",
        Station_Name == "839" ~ "The Basin",
        Station_Name == "716" |Station_Name == "0716"  ~ "0716 Center",
        Station_Name == "1308" ~ "Jewers Bay",
        Station_Name == "Beaver point"  ~ "Beaver Point",
        Station_Name == "Bear Island" & Depl_Date == as_date("2021-05-14") ~ "5005",
        Station_Name == "Big point pond"  ~ "Big Pond Point",
        Station_Name == "Burnt island"  ~ "Burnt Island",
        Station_Name == "Larry's River" ~ "Larrys River",
        Station_Name == "Long beach" ~ "Long Beach",
        Station_Name == "Long Island two" ~ "Long Island 2",
        Station_Name == "McNutt's Island" ~ "McNutts Island",
        Station_Name == "Nyanza Bay West" ~ "Nyanza Bay - W",
        Station_Name == "Nyanza Bay East" ~ "Nyanza Bay - E",
        Station_Name == "Monks head" ~ "Monks Head",
        Station_Name == "Owls head" | Station_Name == "Owl's Head"  ~ "Owls Head",
        Station_Name == "Pipers lake" ~ "Piper Lake",
        Station_Name ==  "Shut-in Island" ~ "Shut-In Island",
        Station_Name ==  "South side" | Station_Name == "South Side" ~ "Southside",
        Station_Name ==  "St. Andrew's Channel - E"|
          Station_Name == "St Andrews Channel - E" ~ "St. Andrews Channel - E",
        Station_Name == "St Peters Canal - S" ~ "St. Peters Canal - S",
        Station_Name == "St Peters Canal - N" |
          Station_Name == "St Peters Canal North" |
          Station_Name == "St. Peters Canal North"~ "St. Peters Canal - N",
        Station_Name == "Tor Bay Center" ~ "Center Bay",
        Station_Name == "Deny's Basin East" ~ "Denys Basin East",
        Station_Name == "Deny's Basin West" ~ "Denys Basin West",
        Station_Name == "193" ~ "0193",
        WATERBODY == "Merigomish Harbour" &
          Station_Name == "Burnt Island" ~ "Back Harbour",
        Station_Name == "Little Narrows - S (Whyc.Bay)" ~ "Little Narrows - S",
        Station_Name == "The Basin South" &
          Depl_Date == as_date("2021-09-08") ~ "The Basin",
       # Station_Name == "St. Peters Canal - N" ~ "St. Peters Canal North",
       # Station_Name == "St. Peters Canal - S" ~ "St. Peters Canal South",
        TRUE ~ Station_Name
      )
    ) %>%
    select(-Waterbody, -Station_Name) %>%
    # CMAR corrected some Waterbodies and re-named some stations
    mutate(
      WATERBODY = case_when(
        WATERBODY == "Tor Bay" & STATION == "Bald Rock" ~ "Whitehead Harbour",
        WATERBODY == "Whycocomagh Bay" &
          (STATION == "Deep Basin" |
             STATION == "0814x East" |
             STATION == "0814x West") ~ "Whycocomagh Basin",
        WATERBODY == "Antigonish Harbour" & STATION == "Captains Pond" ~ "Grahams Cove",
        WATERBODY == "Antigonish Harbour" & STATION == "Monks Head" ~ "Monks Head Harbour",
        WATERBODY == "Pictou Harbour" & STATION == "Melmerby Beach" ~ "Little Harbour",
        TRUE ~ WATERBODY
      ),
      STATION = case_when(
        WATERBODY == "Salt Bay" & STATION == "Salt Bay" ~ "Big Sluice",
        WATERBODY == "Hourglass Lake" & STATION == "Deep Point" ~ "Hourglass Lake",
        WATERBODY == "Arichat Harbour" & STATION == "667" ~ "0667",
        WATERBODY == "Strait of Canso" & STATION == "Pulp Mill 2" ~ "Pulp Mill Site 2",
        WATERBODY == "St. Mary's Bay" &
          (STATION == "Sandy Cove" | STATION == "Sandy cove")
        ~ "Sandy Cove St. Mary's",
        WATERBODY == "Chedabucto Bay" & STATION == "Sandy Cove" ~ "Sandy Cove Chedabucto",
        WATERBODY == "Strait of Canso" & STATION == "Loch" ~ "Canso Lock",
        TRUE ~ STATION
      )
    ) %>%
    mutate(
      MOORING = if_else(Surface.Float. == "Y", "float", "fixed"),

      # fix deployment/recovery dates
      Depl_Date = case_when(

        WATERBODY == "Country Harbour" & STATION == "0001" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),

        WATERBODY == "Country Harbour" & STATION == "0622" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),

        WATERBODY == "Country Harbour" & STATION == "West Ferry Ramp" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),

        WATERBODY == "Marie Joseph Harbour" & STATION == "0028" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),

        WATERBODY == "Marie Joseph Harbour" & STATION == "Jewers Bay" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),

        WATERBODY == "Wine Harbour" & STATION == "Wine Harbour" &
          Depl_Date == as_date("2015-07-28") ~ as_date("2015-09-08"),

        WATERBODY == "Whitehead Harbour" &
          Depl_Date == as_date("2015-07-29") ~ as_date("2015-09-08"),

        WATERBODY == "Chedabucto Bay" & STATION == "Rook Island" &
          Depl_Date == as_date("2020-05-20") ~ as_date("2020-05-21"),

        WATERBODY == "Dover Bay" & STATION == "Seal Cove" &
          Depl_Date == as_date("2021-09-14") ~ as_date("2021-09-15"),

        WATERBODY == "St. Mary's Bay" & STATION == "Church Point 1" &
          Depl_Date == as_date("2020-02-24") ~ as_date("2020-02-23"),

        WATERBODY == "St. Mary's Bay" & STATION == "Church Point 2" &
          Depl_Date == as_date("2020-02-24") ~ as_date("2020-02-23"),

        TRUE ~ Depl_Date
      ),

      Recv_Date = case_when(
        WATERBODY == "Chedabucto Bay" & STATION == "Rook Island" &
          Recv_Date == as_date("2020-05-20") ~ as_date("2020-05-21"),

       STATION == "Barren Island" &  Recv_Date == as_date("2022-08-16") ~
         as_date("2022-08-17"),
        TRUE ~ Recv_Date
      )
    ) %>%
    select(WATERBODY, STATION, Depl_Date, Recv_Date, MOORING) %>%
    # remove 7 duplicate rows
    distinct() %>%
    mutate(
      MOORING = case_when(
        STATION == "Wine Harbour" & Depl_Date == as_date("2015-09-08") ~ "float",
        TRUE ~ MOORING
      )
    )
}


# x <- dat_raw %>%
#   filter(!is.na(Recv_Date), Recv_Date != "") %>%
#   mutate(Recv_Date = dmy(Recv_Date))
#
# filter(x, is.na(Recv_Date))







