#'@importFrom dplyr add_row filter
#'@importFrom lubridate year month day hour hours

# extracts observations for the hour in November when daylight savings ends
# called by convert_HOBO_datetime_to_real_UTC()

dates_to_fix <- function(hobo.data, year.DST, day.DST) {

  hobo.data %>%
    # filter for the year, November, the day that DST ended in that year, and 1 am
    # this will extract two observations for each sampling increment throughout the 1 am hour
    ## e.g., 1:00, 1:30, 1:00, 1:30
    ## the first set of observations should be for the hour before: 0:00, 0:30
    filter(year(UTC) == year.DST, month(UTC) == 11, day(UTC) == day.DST, hour(UTC) == 1) %>%
    mutate(n_fix = c(1:n()),                   # add an index column from 1:number of observations found
           inx_fix = n_fix/max(n_fix)) %>%     # divide index column by 2 (values after 0.5 are the real 1am)
    # subtract one hour for the observations with n_fix_max <= 0.5
    mutate(UTC_fix = if_else(inx_fix <= 0.5, UTC - hours(1), UTC))
}



# convert_HOBO_datetime_to_real_UTC ---------------------------------------


#'@importFrom lubridate force_tz dst %within% interval hours ymd
#'@import dplyr

convert_HOBO_datetime_to_true_UTC <- function(HOBO.DATA) {

  hobo <- HOBO.DATA %>%
    mutate(ADT_force = force_tz(TIMESTAMP, tzone = "America/Halifax"),
           # identify which datetimes are in daylight savings
           DAYLIGHT_SAVINGS = dst(ADT_force),
           # if in daylight savings, subtract 1 hour
           UTC = if_else(DAYLIGHT_SAVINGS == TRUE, TIMESTAMP - hours(1), TIMESTAMP),
           INDEX = c(1:n()))

  # date range of observations
  date.range <- interval(min(hobo$UTC), max(hobo$UTC))

  # check if the END of daylight savings is in the data range
  # if so, there will be duplicate obs for the 1 am hour. Use dates_to_fix()

  if(ymd("2021-11-07") %within% date.range){
    year.fix <- 2021
    day.fix <- 07

    fix_index <- dates_to_fix(hobo, year.DST = year.fix, day.DST = day.fix)
    hobo[which(hobo$INDEX %in% fix_index$INDEX), "UTC"] <- fix_index$UTC_fix
  }

  if(ymd("2020-11-01") %within% date.range){
    year.fix <- 2020
    day.fix <- 01

    fix_index <- dates_to_fix(hobo, year.DST = year.fix, day.DST = day.fix)
    hobo[which(hobo$INDEX %in% fix_index$INDEX), "UTC"] <- fix_index$UTC_fix
  }

  if(ymd("2019-11-03") %within% date.range){
    year.fix <- 2019
    day.fix <- 03

    fix_index <- dates_to_fix(hobo, year.DST = year.fix, day.DST = day.fix)
    hobo[which(hobo$INDEX %in% fix_index$INDEX), "UTC"] <- fix_index$UTC_fix
  }

  if(ymd("2018-11-04") %within% date.range){
    year.fix <- 2018
    day.fix <- 04

    fix_index <- dates_to_fix(hobo, year.DST = year.fix, day.DST = day.fix)
    hobo[which(hobo$INDEX %in% fix_index$INDEX), "UTC"] <- fix_index$UTC_fix
  }

  if(ymd("2017-11-05") %within% date.range){
    year.DST <- 2017
    day.DST <- 05

    fix_index <- dates_to_fix(hobo, year.DST = year.fix, day.DST = day.fix)
    hobo[which(hobo$INDEX %in% fix_index$INDEX), "UTC"] <- fix_index$UTC_fix
  }

  if(ymd("2016-11-06") %within% date.range){
    year.DST <- 2016
    day.DST <- 06

    fix_index <- dates_to_fix(hobo, year.DST = year.fix, day.DST = day.fix)
    hobo[which(hobo$INDEX %in% fix_index$INDEX), "UTC"] <- fix_index$UTC_fix
  }

  if(ymd("2015-11-08") %within% date.range){
    year.DST <- 2015
    day.DST <- 08

    fix_index <- dates_to_fix(hobo, year.DST = year.fix, day.DST = day.fix)
    hobo[which(hobo$INDEX %in% fix_index$INDEX), "UTC"] <- fix_index$UTC_fix
  }


  hobo %>%
    select(INDEX, TIMESTAMP = UTC, TEMPERATURE)

}




