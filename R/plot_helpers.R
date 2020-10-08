#'@title Returns nice major and minor breaks and label format based on timespan of the data
#'@details This is a temporary function to help prepare OpenData reports.
#'@param timespan Number of days of data to be plotted.
#'@return Returns a dataframe with 1 observation of 3 variables
#'  \code{date.breaks.major}, \code{date.breaks.minor},
#'  \code{date.labels.format}.

#'@family OpenData
#'@author Danielle Dempsey
#'@export

# get_xaxis_breaks() ----------------------------------------------

# returns nice major and minor breaks and label format based on timespan of the data

# timespan: number of days of data

# returns a dataframe with 1 observation of 3 variables
## (date.breaks.major,date.breaks.minor, date.labels.format)

get_xaxis_breaks <- function(timespan){

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

  if(timespan > 120){
    date.breaks.major = "2 month"
    date.breaks.minor = "1 month"
    date.labels.format = "%y-%b"
  }

  data.frame(date.breaks.major = date.breaks.major,
             date.breaks.minor = date.breaks.minor,
             date.labels.format = date.labels.format)

}
