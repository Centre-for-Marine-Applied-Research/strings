#' Add MOORING column to tidydata for submission to Open Data Portal
#'
#' @param dat Dataframe, as returned from \code{assemble_county_data()}. Must
#'   include columns \code{WATERBODY},\code{STATION}, and
#'   \code{DEPLOYMENT_PERIOD}.
#'
#' @param path_mooring Path to the mooring type table (including file name and
#'   extension).
#'
#' @return Returns \code{dat} with additional column \code{MOORING}.
#'
#' @importFrom dplyr anti_join left_join mutate select
#' @importFrom tidyr separate
#' @importFrom lubridate as_date
#' @importFrom glue glue
#'
#' @export


add_mooring_column <- function(dat, path_mooring = NULL){

  if(is.null(path_mooring)){

    path_mooring <- file.path("Y:/Coastal Monitoring Program/Open Data/Deployment Data/MOORING_table.csv")

  }

  mooring <- read_mooring_csv(path_mooring)


  dat <- dat %>%
    separate(DEPLOYMENT_PERIOD,
             into = c("Depl_Date", NA), sep = " to", remove = FALSE) %>%
    mutate(Depl_Date = as_date(Depl_Date))

  # check that all deployments in dat are in mooring table
  check <- dat %>%
    anti_join(mooring, by = c("WATERBODY","STATION", "Depl_Date")) %>%
    distinct(WATERBODY, STATION, Depl_Date)

  if(nrow(check) > 0){

    warning(glue("{nrow(check)} deployments in dat not found in mooring table"))

    print(check)
  }

  dat %>%
    left_join(mooring, by = c("WATERBODY","STATION", "Depl_Date")) %>%
    select(-Depl_Date, -Recv_Date)

}
