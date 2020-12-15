# format_date_for_dd() ----------------------------------------------

# function to format start and end dates for the calculate_degree_days() function

# x.date is a character string that can be converted to a date OR a datetime object
## See parse.orders for possible input format
# day1: TRUE if x.date is the start date (will add 0:00:00 if no time supplied in x.date)
## FALSE if x.data is the end date (will add 23:59:59 if no time supplied in x.date)

# returns a datetime object

#'@importFrom lubridate parse_date_time ymd_hm ymd_hms

format_date_for_dd <- function(x.date, day1 = TRUE){

  parse.orders <- c("ymd IMS p", "Ymd IMS p",
                    "Ymd HM", "Ymd HMS",
                    "dmY HM", "dmY HMS",
                    "Ymd", "ymd")

  # parse x.date OR if the format is not correct, return an error
  if(!is.na(suppressWarnings(parse_date_time(x.date, orders = parse.orders)))){

    x.date <- lubridate::parse_date_time(x.date, orders = parse.orders)

  } else {
    stop("start.date or end.date is not in a format recognized by the strings package.
           See help files for more information.")
  }

  # if x.date does not have a time component, paste "00:00:00" for the start date or 23:59:59 for the end date
  if(suppressWarnings(is.na(ymd_hms(x.date))) & suppressWarnings(is.na(ymd_hm(x.date))) ) {

    if(day1 == TRUE) x.date <- as_datetime(paste(x.date, "00:00:00"))
    if(day1 == FALSE) x.date <- as_datetime(paste(x.date, "23:59:59"))
  }

  as_datetime(x.date)
}
