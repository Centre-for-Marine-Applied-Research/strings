#'@title Converts compiled string data to a tidy format
#'@param dat.wide Data in the format exported by the \code{compile_xxx_data}
#'  functions.
#'@param remove.NA Logical value. If \code{remove.NA = TRUE}, observations where
#'  \code{DATE} or \code{VALUE} = \code{NA} are removed. Default is
#'  \code{remove.NA = TRUE}.
#'@param show.NA.message Logical value. If \code{show.NA.message = TRUE}, the
#'  number of NA values removed is printed to the console. Default is
#'  \code{show.NA.message = FALSE}.
#'@return Returns a tidy dataframe with six columns: \code{DATE} (POSIXct),
#'  \code{VALUE}, (numeric), \code{DATE_RANGE} (character), \code{SENSOR},
#'  \code{VARIABLE} (character), \code{DEPTH} (ordered factor).
#'@family format
#'@author Danielle Dempsey
#'@importFrom lubridate parse_date_time
#'@importFrom readr parse_number
#'@importFrom readxl read_excel
#'@importFrom stats na.omit
#'@importFrom tidyr separate
#'@import dplyr
#'@export

convert_to_tidydata <- function(dat.wide, remove.NA = TRUE, show.NA.message = FALSE){

  if(ncol(dat.wide) %% 2 != 0) {
    stop(
    "ERROR: dat.wide has an odd number of columns. dat.wide should have an even number of columns, alternating between a column of dates and a corresponding column of values.")
  }

  ind <- seq(1, ncol(dat.wide), 2)    # index for every second column
  dat.tidy <- data.frame(NULL)        # initialize dat.tidy

  for(i in ind){

    dat.i <- dat.wide[, c(i, i+1)]           # subset to columns of interest (should be 1 date column, 1 value column)

    # check if there is data in the date column.
    # If the whole column is NA, dat.i.tidy is set to NULL, and the loop moves on to the next i
    if(sum(is.na(dat.i[, 1])) == (nrow(dat.i) - 4)){  # nrow(dat.i) - 4 to account for the metadata in the first 4 rows of the dataframe

      dat.i.tidy <- data.frame(NULL)

    } else{

      daterange <- as.character(dat.i[1,1])    # extract the date range

      sensor <- as.character(dat.i[2,1])       # extract the sensor (type and serial number)

      # extract the variable and depth and split into two colummns
      var_depth <- dat.i[3,1] %>%
        data.frame() %>%
        rename(var_depth = 1)  %>%
        separate(var_depth, into = c("variable", "depth"), sep = "-| - ") # can handle "variable-depth" OR "variable - depth"

      variable <- as.character(var_depth[1])
      depth <-  parse_number(as.character(var_depth[2]))

      # compile the tidy data
      dat.i.tidy <- dat.i %>% slice(-c(1:4)) %>%      # remove first four rows of data
        select(DATE = 1, VALUE = 2)                   # name column 1 DATE and column 2 VALUE

      # if the date can be converted to class numeric, then it is stored as a number in Excel
      ## and we have to use janitor::convert_to_datetime to convert to POSIXct.
      # Otherwise the date should be a character string that can be converted to POSIXct using
      ## lubridate::parse_date_time (might have to add orders = c("Ymd HM", "Ymd HMS")))
      date_format <- dat.i.tidy$DATE[1]
      if(!is.na(suppressWarnings(as.numeric(date_format)))) {

        dat.i.tidy <-  dat.i.tidy %>%
          mutate(DATE = convert_to_datetime(as.numeric(DATE)))

        } else {

        dat.i.tidy <-  dat.i.tidy %>%
          mutate(DATE = parse_date_time(DATE, orders = "Ymd HMS"))

      }

      dat.i.tidy <- dat.i.tidy %>%
        mutate(DATE_RANGE = daterange,        # add DATE_RANGE column
               SENSOR = sensor,               # Add SENSOR column
               VARIABLE = variable,           # Add VARIABLE column
               DEPTH = factor(depth),         # Add DEPTH column (should be a factor or else the color scheme will be a gradient)
               VALUE = as.numeric(VALUE))
    }

    dat.tidy <- rbind(dat.tidy, dat.i.tidy)         # bind dat.tidy with dat.i.tidy

  }

  # convert DEPTH to an ORDERED factor (so 2 < 5 < 10 etc., no matter what order they appear in dat.wide)
  dat.tidy <- dat.tidy %>%
    mutate(DEPTH = ordered(DEPTH,
                           levels = as.numeric(levels(DEPTH))[order(as.numeric(levels(DEPTH)))]))


  # number of NAs in the DATE and VALUE columns
  date.na <- sum(is.na(dat.tidy$DATE))
  value.na <- sum(is.na(dat.tidy$VALUE))

  if(remove.NA == TRUE){

    dat.tidy <- data.frame(na.omit(dat.tidy))

    if(show.NA.message == TRUE) print(paste("NOTE:", sum(date.na), "NA values removed from DATE column.", sum(value.na), "NA values removed from VALUE column"))

  } else {
    if(show.NA.message == TRUE) print(paste("NOTE:", sum(date.na), "NA values in DATE column.", sum(value.na), "NA values in VALUE column"))
  }

  dat.tidy # return dat.tidy

}
