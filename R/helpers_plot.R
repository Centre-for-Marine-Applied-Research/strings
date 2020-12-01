#'@title Returns nice major and minor breaks and label format based on timespan
#'  of the data
#'@details If the timespan in the \code{TIMESTAMP} column of \code{dat.tidy} is
#'  less than or equal to 60 days, the major and minor breaks will be every 2
#'  weeks; if time span is greater than 60 days and less than or equal to 120
#'  days, the major and minor breaks will be every month; if time span is
#'  greater than 120 days and less than or equal to 240 days, the major breaks
#'  will be 2 months and the minor breaks will be 1 month; if the time span is
#'  greater than 240 days, the major breaks will be 4 months and the minor
#'  breaks will be 1 month.
#'@param dat.tidy Data to be plotted, in tidy format, as returned by the
#'  function \code{convert_to_tidydata()}. Must include the column
#'  \code{TIMESTAMP} (POSIXct).
#'@return Returns a dataframe with 1 observation of 3 variables
#'  \code{date.breaks.major}, \code{date.breaks.minor},
#'  \code{date.labels.format}.

#'@family plot
#'@author Danielle Dempsey
#'@export


get_xaxis_breaks <- function(dat.tidy){

  # timespan of the data
  timespan <- difftime(max(dat.tidy$TIMESTAMP), min(dat.tidy$TIMESTAMP), units = "days")
  timespan <- unclass(timespan)[1]

  if(timespan <= 60){
    date.breaks.major = "2 week"
    date.breaks.minor = "2 week"
    date.labels.format = "%y-%b-%d"
  }

  if(60 < timespan & timespan <= 120){
    date.breaks.major = "1 month"
    date.breaks.minor = "1 month"
    date.labels.format = "%y-%b"
  }

  if(timespan > 120 & timespan <= 240){
    date.breaks.major = "2 month"
    date.breaks.minor = "1 month"
    date.labels.format = "%y-%b"
  }

  if(timespan > 240){
    date.breaks.major = "4 month"
    date.breaks.minor = "1 month"
    date.labels.format = "%y-%b"
  }

  data.frame(date.breaks.major = date.breaks.major,
             date.breaks.minor = date.breaks.minor,
             date.labels.format = date.labels.format)

}
