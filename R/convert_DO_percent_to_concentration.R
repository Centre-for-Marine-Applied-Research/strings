#'@title Converts dissolved oxygen from percent to concentration (mg/L)
#'@param dat.tidy Tidy data **fill in here
#'@param pressure Nicole to send blurb
#'@return Fill this in. Returns a tidy dataframe with six columns: \code{DATE} (POSIXct),
#'  \code{VALUE}, (numeric), \code{DATE_RANGE} (character), \code{SENSOR},
#'  \code{VARIABLE} (character), \code{DEPTH} (ordered factor).
#'@family format
#'@author Danielle Dempsey
#'@importFrom tidyr pivot_wider pivot_longer separate
#'@import dplyr
#'@export

convert_DO_percent_to_concentration <- function(dat.tidy, pressure){


  dat <- dat.tidy %>%
    separate(SENSOR, into = c("SENSOR", NA), sep = "-") %>%
    filter(SENSOR == "aquaMeasure") %>%
    pivot_wider(names_from = VARIABLE, values_from = VALUE) %>%
    mutate(
      cp = ((exp(7.7117-1.31403*log(Temperature+45.93))) *
                   pressure *
                   (1-exp(11.8571-(3840.7/(Temperature+273.15))-
                            (216961/((Temperature+273.15)^2)))/pressure) *
                   (1-(0.000975-(0.00001426*Temperature)+(0.00000006436*(Temperature^2)))*pressure)) /
             (1-exp(11.8571-(3840.7/(Temperature+273.15))-(216961/((Temperature+273.15)^2)))) /
             (1-(0.000975-(0.00001426*Temperature)+(0.00000006436*(Temperature^2))))
    ) %>%
    mutate(DO_concentration = (cp* `Dissolved Oxygen`)/100) %>%
    select(-cp) %>%
    pivot_longer(cols= c("Temperature", "Dissolved Oxygen", "DO_concentration"),
                 names_to = "VARIABLE", values_to = "VALUE")


}


