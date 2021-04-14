#' Filters out data from days when the value exceeds a threshold
#'
#' @description Filters out all data from days when the value exceeds a given
#'   threshold for a given number of observations. Option to filter out
#'   additional days after the threshold is exceeded.
#'
#'   \code{dat.tidy } should only include data for one variable.
#'
#' @param dat.tidy Dataframe with at least three columns: \code{TIMESTAMP} (must
#'   be possible to convert to a Date object), \code{DEPTH}, and \code{VALUE}.
#'   If column \code{VARIABLE} is included, it must have one unique entry. Other
#'   columns will be ignored.
#'
#' @param threshold The threshold to trigger filtering data (inclusive. Default
#'   is \code{threshold = 20}.
#' @param min.exceedence The minimum number of observations in a day to trigger
#'   filtering data (inclusive). Default is \code{min.exceedence = 1}.
#' @param n.days.to.filter Number of days to filter out when \code{threshold}
#'   and \code{min.exceedence} are exceeded. If \code{n.days.to.filter = 1} (the
#'   default), then all values from the day the threshold is exceeded will be
#'   filtered out. If \code{n.days.to.filter = 2}, then all values from the day
#'   the threshold is exceeded and the following day will be filtered out, etc.
#'
#' @return Returns dat.tidy, filtered when triggered by \code{threshold} and
#'   \code{min.exceedence}.
#'
#' @importFrom dplyr mutate filter if_else group_by summarise
#' @importFrom lubridate as_date
#'
#' @export

filter_days <- function(dat.tidy,
                        threshold = 20,
                        min.exceedence = 1,
                        n.days.to.filter = 1) {


  if("VARIABLE" %in% colnames(dat.tidy)){

    if(length(unique(dat.tidy$VARIABLE)) > 1) {

      stop("More than one VARIABLE found in dat.tidy. \n
         HINT: filter dat.tidy for the variable of interest before applying
         filter_days()")
    }
  }

  dat.tidy <- dat.tidy %>%
    mutate(DATE = as_date(TIMESTAMP))

  dat.tidy %>%
    # table of DEPTH and the DATE for which the threshold is exceeded
    # anti-join to remove these rows from dat.tidy
    anti_join(
      dat.tidy %>%
        mutate(
          DATE = as_date(TIMESTAMP),
          EXCEED_THRESH = if_else(VALUE >= threshold, TRUE, FALSE)
        ) %>%
        group_by(DEPTH, DATE) %>%
        summarise(n_obs = sum(EXCEED_THRESH)) %>%
        filter(n_obs >= min.exceedence),
      by = c("DEPTH", "DATE")
    ) %>%
    select(-DATE)

}
