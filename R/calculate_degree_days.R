#'@title Converts temperature data to degree-days
#'@details add equation here
#'@param dat.tidy Data in tidy format, as returned by the function
#'  \code{convert_to_tidydata()}. Must include three columns: \code{DATE}
#'  (POSIXct), \code{VARIABLE} (character), and \code{VALUE} (numeric). May also
#'  include a column \code{DEPTH} (factor), which can be used to filter the data
#'  included in the degree-days calculation. Other columns wil be ignored.
#'@param start.date First day that should be included in the calculation. If
#'  \code{start.date} does not include a time, then the start time is assumed to
#'  be midnight. Accepted orders for \code{start.date} are: "ymd IMS p", "Ymd
#'  IMS p", "Ymd HM", "Ymd HMS", "dmY HM", "dmY HMS", "Ymd", "ymd". Default is
#'  \code{start.date = min(dat.tidy$DATE)}.
#'@param end.date Last day that should be included in the calculation. If
#'  \code{end.date} does not include a time, then the end time is assumed to be
#'  "23:59:59". Accepted orders for \code{start.date} are: "ymd IMS p", "Ymd IMS
#'  p", "Ymd HM", "Ymd HMS", "dmY HM", "dmY HMS", "Ymd", "ymd". Default is
#'  \code{start.date = max(dat.tidy$DATE)}.
#'@param depths.to.include Numeric vector of depths to include in the
#'  calculation. Default is \code{depths.to.include = "ALL"}, which includes all
#'  depths in the \code{DEPTH} column of \code{dat.tidy}.
#'@param group.by.depth Logical value. By default, \code{group.by.depth = TRUE},
#'  and so the degree days are calculated and returned for each depth specified
#'  in \code{depths.to.include}. If \code{group.by.depth = FALSE} the
#'  degree-days will averaged over all depths.
#'@return If \code{group.by.depth = FALSE}, returns the calue of the calculated
#'  degree-days averaged over all depths specified in \code{depths.to.include}.
#'  If \code{group.by.depth = TRUE}, returns a tibble with two columns:
#'  \code{DEPTH}, and the corresponding \code{Degree_Days}.
#'@family calculate
#'@author Danielle Dempsey
#'@import dplyr
#'@importFrom lubridate parse_date_time ymd_hm ymd_hms
#'@export

calculate_degree_days <- function(dat.tidy,
                                  start.date = min(dat.tidy$DATE),
                                  end.date = max(dat.tidy$DATE),
                                  depths.to.include = "ALL", group.by.depth = TRUE){

  parse.orders <- c("ymd IMS p", "Ymd IMS p",
                    "Ymd HM", "Ymd HMS",
                    "dmY HM", "dmY HMS",
                    "Ymd", "ymd")

  #  Format start.date ------------------------------------------------------

  # parse start.date OR if the format is not correct, return an error
  if(!is.na(suppressWarnings(parse_date_time(start.date, orders = parse.orders)))){

    start.date <- lubridate::parse_date_time(start.date, orders = parse.orders)

  } else {
    stop("start.date is not in a format recognized by the strings package.
           See help files for more information.")
  }

  # if start.date does not have a time component, paste "00:00:00"
  if(suppressWarnings(is.na(ymd_hms(start.date))) & suppressWarnings(is.na(ymd_hm(start.date))) ) {

    start.date <- as_datetime(paste(start.date, "00:00:00"))
  }


  # Format end.date ---------------------------------------------------------

  # parse end.date OR if the format is not correct, return an error
  if(!is.na(suppressWarnings(parse_date_time(end.date, orders = parse.orders)))){

    end.date <- lubridate::parse_date_time(end.date, orders = parse.orders)

  } else {
    stop("end.date is not in a format recognized by the strings package.
           See help files for more information.")
  }

  # if end.date does not have a time component, paste "23:59:59"
  if(suppressWarnings(is.na(ymd_hms(end.date))) & suppressWarnings(is.na(ymd_hm(end.date))) ) {

    end.date <- as_datetime(paste(end.date, "23:59:59"))
  }

  # calculate the number of days in the time span (difference + 1)
  diff_in_days <- round(as.numeric(difftime(end.date, start.date, units = "days")), digits = 3) + 1

  # filter to the dates of interest
  dat.tidy <- dat.tidy %>%
    filter(VARIABLE == "Temperature",
           DATE >= start.date,
           DATE <= end.date)

  # if depths.to.include is not "ALL", filter to the specified depths
  if(depths.to.include[1] != "ALL") {
    dat.tidy <- dat.tidy %>%
      mutate(DEPTH = as.numeric(as.character(DEPTH))) %>% # convert DEPTH from factor to character to numeric
      filter(DEPTH %in% depths.to.include)
  }

  # if group.by.depth = TRUE, use the group_by() function; otherwise calculate Degree_Days directly
  if(group.by.depth == TRUE){
    Degree_Days <- dat.tidy %>%
      group_by(DEPTH) %>%
      summarize(Degree_Days = round((mean(VALUE) * diff_in_days), digits = 3)) %>%
      ungroup()

  } else{ Degree_Days <- round((mean(dat.tidy$VALUE) * diff_in_days), digits = 3) }

 # print(paste("degree day from ", start.date, " to ", end.date, " at depths.to.include(s): ", depth, paste = "" ))

  Degree_Days

}









