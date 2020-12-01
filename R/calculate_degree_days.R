#'@title Calculates temperature in degree-days
#'@details Degree-days = average temperature over \emph{n} days * \emph{n} days
#'
#'  \emph{n} is calculated using the \code{difftime()} function:
#'
#'  \code{n_DAYS = round(as.numeric(difftime(max(TIMESTAMP), min(TIMESTAMP),
#'  units = "days")), digits = 3)}
#'
#'@param dat.tidy Data in tidy format, as returned by the function
#'  \code{convert_to_tidydata()}. Must include three columns: \code{TIMESTAMP}
#'  (POSIXct), \code{VARIABLE} (character), and \code{VALUE} (numeric). May also
#'  include columns with grouping variables passed to \code{...}. Other
#'  columns will be ignored.
#'
#'@param start.date First day to include in the calculation. If
#'  \code{start.date} does not include a time, then the start time is assumed to
#'  be midnight. Accepted orders for \code{start.date} are: "ymd IMS p", "Ymd
#'  IMS p", "Ymd HM", "Ymd HMS", "dmY HM", "dmY HMS", "Ymd", "ymd". Default is
#'  the first TIMESTAMP in \code{dat.tidy}.
#'@param end.date Last day to include in the calculation. If \code{end.date}
#'  does not include a time, then the end time is assumed to be "23:59:59".
#'  Accepted orders for \code{end.date} are: "ymd IMS p", "Ymd IMS p", "Ymd HM",
#'  "Ymd HMS", "dmY HM", "dmY HMS", "Ymd", "ymd". Default is the last TIMESTAMP
#'  in \code{dat.tidy}.
#'@param ... Columns in \code{dat.tidy} to use for grouping in
#'  \code{dplyr::group_by()}, e.g., \code{YEAR, MONTH}. Degree-days are
#'  calculated for each combination of groups.
#'@return Returns a tibble with at least five columns: \code{PERIOD} (start and
#'  end date used to calculate mean temperature and number of days),
#'  \code{n_DAYS} (the number of days, \emph{n}, used in the calculation),
#'  \code{n_OBSERVATIONS} (the number of observations used to calculate the
#'  average temperature), \code{AVG_TEMPERATURE} (the average temperature in the
#'  time period), \code{DEGREE_DAYS} (degree-days, the product of
#'  \code{AVG_TEMPERATURE} and \code{n_DAYS}). Additional columns are returned
#'  for each grouping variable in \code{...}.

#'@family calculate
#'@author Danielle Dempsey
#'@import dplyr
#'@export

#' @examples
#' data(tidydata)
#'
#' # degree-days averaged over all depths and the whole time series
#' calculate_degree_days(tidydata)
#'
#' # degree-days by DEPTH for whole time series
#' calculate_degree_days(tidydata, DEPTH)
#'
#' # degree-days by DEPTH and month from July 1 to September 30
#' calculate_degree_days(tidydata, DEPTH, months(TIMESTAMP),
#' start.date = "2019-06-01", end.date = "2019-09-30")

calculate_degree_days <- function(dat.tidy,
                                  ...,
                                  start.date = min(dat.tidy$TIMESTAMP),
                                  end.date = max(dat.tidy$TIMESTAMP)
                                  ){

  # format dates (convert to datetime object and add time if only date was supplied)
  start.date <- format_date_for_dd(start.date, day1 = TRUE)
  end.date <- format_date_for_dd(end.date, day1 = FALSE)

  # filter to the dates of interest
  dat.tidy <- dat.tidy %>%
    filter(VARIABLE == "Temperature",
           TIMESTAMP >= start.date,
           TIMESTAMP <= end.date)

  # calculate and return degree-days
  dat.tidy %>%
    # group by the columns specified in ...
    group_by(...) %>%
    summarise(START_DAY = format(min(TIMESTAMP), "%Y-%b-%d"),
              END_DAY = format(max(TIMESTAMP), "%Y-%b-%d"),
              # number of observations in each group
              n_OBSERVATIONS = n(),
              # number of days in group
              n_DAYS = round(as.numeric(difftime(max(TIMESTAMP), min(TIMESTAMP), units = "days")), digits = 3),
              # average temperature in group
              AVG_TEMPERATURE =  round(mean(VALUE), digits = 3)) %>%
    mutate(DEGREE_DAYS = round(n_DAYS * AVG_TEMPERATURE, digits = 0),
           n_DAYS = round(n_DAYS, digits = 1)) %>%
    ungroup() %>%
    select(START_DAY, END_DAY, n_DAYS, everything()) %>%
    arrange(parse_date_time(START_DAY, orders = "Ymd"))

}









