#'@title Converts compiled string data to a tidy format
#'@details I haven't done anything with the DATES. Still need to un-account for
#'  daylight savings. To be discussed. if there is no data in the Date column
#'  (e.g., column 15 for Beaver Point), this column and the corresponding
#'  variable column are skipped (these NAs are NOT included in the NA count
#'  provided by the message)
#'@param dat.wide Data in the format exported by \code{compile_HOBO_data} and
#'  \code{compile_aquaMeasure_data}.
#'@param remove.NA Logical value. If \code{remove.NA = TRUE}, observations where
#'  \code{DATE} or \code{VALUE} = \code{NA} are removed. Default is
#'  \code{remove.NA = TRUE}.
#'@param show.NA.message Logical value. If \code{show.NA.message = TRUE}, the
#'  number of NA values removed is printed to the console.
#'@return Returns a tidy dataframe with six columns: \code{DATE} (POSIXct),
#'  \code{VALUE}, (numeric), \code{DATE_RANGE} (character), \code{SENSOR},
#'  \code{VARIABLE} (character), \code{DEPTH} (factor).
#'@family compile
#'@author Danielle Dempsey
#'@importFrom lubridate parse_date_time
#'@importFrom readr parse_number
#'@importFrom readxl read_excel
#'@importFrom stats na.omit
#'@importFrom tidyr separate
#'@import dplyr
#'@export




convert_to_tidydata <- function(dat.wide, remove.NA = TRUE, show.NA.message = TRUE){
  
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
      
      sensor <- as.character(dat.i[2,1])       # extract the sensor
      
      # extract the variable and depth and split into two colummns
      var_depth <- dat.i[3,1] %>% 
        data.frame() %>% 
        rename(var_depth = 1)  %>% 
        separate(var_depth, into = c("variable", "depth"), sep = "-")
      
      variable <- as.character(var_depth[1])
      depth <-  parse_number(as.character(var_depth[2]))
      
      
      dat.i.tidy <- dat.i %>% slice(-c(1:4)) %>%      # remove first four rows of data
        select(DATE = 1, VALUE = 2) %>%               # name column 1 DATE and column 2 VALUE
        mutate(DATE_RANGE = daterange,      
          DATE = parse_date_time(DATE, orders = "Ymd HMS"),
          VARIABLE = variable,                   # Add VARIABLE column
          DEPTH = factor(depth),                 # Add DEPTH column (should be a factor or else the color scheme will be a gradient)
          VALUE = as.numeric(VALUE))
    }
    
    dat.tidy <- rbind(dat.tidy, dat.i.tidy)         # bind dat.tidy with dat.i.tidy
    
  }
  
  # number of NAs in the DATE and VALUE columns
  date.na <- sum(is.na(dat.tidy$DATE))
  value.na <- sum(is.na(dat.tidy$VALUE))
  
  if(remove.NA == TRUE){
    
    dat.tidy <- data.frame(na.omit(dat.tidy)) %>%   # remove NAs
      mutate(DEPTH = ordered(DEPTH,                 # convert DEPTH to an ORDERED factor (so 2 < 5 < 10 etc., no matter what order they appear in dat.wide)
                             levels = as.numeric(levels(DEPTH))[order(as.numeric(levels(DEPTH)))]))
    
    if(show.NA.message == TRUE) print(paste("NOTE:", sum(date.na), "NA values removed from DATE column.", sum(value.na), "NA values removed from VALUE column"))
    
  } else { 
    if(show.NA.message == TRUE) print(paste("NOTE:", sum(date.na), "NA values in DATE column.", sum(value.na), "NA values in VALUE column"))
  }
  
  dat.tidy # return dat.tidy
  
}