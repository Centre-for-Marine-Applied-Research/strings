#'@title Calculates temperature in degree-days
#'@details Degree-days = average temperature over \emph{n} days * \emph{n}
#'
#'  \emph{n} is calculated using the \code{difftime()} function:
#'
#'  \code{n_DAYS = round(as.numeric(difftime(max(DATE), min(DATE), units =
#'  "days")), digits = 3)}
#'
#'@param .dat.tidy Data in tidy format, as returned by the function
#'  \code{convert_to_tidydata()}. Must include three columns: \code{DATE}
#'  (POSIXct), \code{VARIABLE} (character), and \code{VALUE} (numeric). May also
#'  include the column \code{DEPTH} (factor) (required if
#'  \code{.depths.to.include} is not set to the default), or other grouping
#'  columns passed to \code{...}. Other columns wil be ignored.
#'@param .start.date First day that should be included in the calculation. If
#'  \code{.start.date} does not include a time, then the start time is assumed
#'  to be midnight. Accepted orders for \code{start.date} are: "ymd IMS p", "Ymd
#'  IMS p", "Ymd HM", "Ymd HMS", "dmY HM", "dmY HMS", "Ymd", "ymd". Default is
#'  \code{.start.date = min(.dat.tidy$DATE)}.
#'@param .end.date Last day that should be included in the calculation. If
#'  \code{.end.date} does not include a time, then the end time is assumed to be
#'  "23:59:59". Accepted orders for \code{start.date} are: "ymd IMS p", "Ymd IMS
#'  p", "Ymd HM", "Ymd HMS", "dmY HM", "dmY HMS", "Ymd", "ymd". Default is
#'  \code{.end.date = max(.dat.tidy$DATE)}.
#'@param ... Columns in \code{.dat.tidy} to use for grouping in
#'  \code{dplyr::group_by()}, e.g., \code{YEAR, MONTH}. Degree-days are
#'  calculated for each combination of groups.
#'@return Returns a tibble with at least five columns: \code{PERIOD} (start and
#'  end date and time used to calculate mean temperature and number of days),
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

calculate_degree_days <- function(.dat.tidy,
                                  ...,
                                  .start.date = min(.dat.tidy$DATE),
                                  .end.date = max(.dat.tidy$DATE)
                                  ){

  # format dates (convert to datetime object and add time if only date was supplied)
  start.date <- format_date_for_dd(.start.date, day1 = TRUE)
  end.date <- format_date_for_dd(.end.date, day1 = FALSE)

  # filter to the dates of interest
  dat.tidy <- .dat.tidy %>%
    filter(VARIABLE == "Temperature",
           DATE >= start.date,
           DATE <= end.date)

  dat.tidy %>%
    group_by(...) %>%                # group by the columns specified in ...
    summarise(START_DAY = format(min(DATE), "%Y-%b-%d %H:%M"),
              END_DAY = format(max(DATE), "%Y-%b-%d %H:%M"),
              n_OBSERVATIONS = n(),  # number of observations in each group
              n_DAYS = round(as.numeric(difftime(max(DATE), min(DATE), units = "days")), digits = 3),     # number of days in group
              AVG_TEMPERATURE =  round(mean(VALUE), digits = 3)) %>%                                      # average temperature in group
    mutate(DEGREE_DAYS = round(.data$n_DAYS * .data$AVG_TEMPERATURE, digits = 0),
           n_DAYS = round(n_DAYS, digits = 1)) %>%  #,
           #PERIOD = paste(START_DAY, "to", END_DAY)) %>%                                 # degree days in each group
    ungroup() %>%
    #select(-START_DAY, -END_DAY) %>%
    #select(PERIOD, n_DAYS, everything())
    select(START_DAY, END_DAY, n_DAYS, everything())

}









