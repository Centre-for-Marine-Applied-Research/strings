#'@title Calculates dissolved oxygen concentration (mg/L)
#'@description Converts dissolved oxygen from percent saturation to
#'  concentration (mg/L) based on temperature and pressure.
#'
#'@details DO concentration is calculated as:
#'
#'  \deqn{DO_{mg/L} = C_p * DO_{\% Saturation} / 100}
#'
#'  To see the calculation for \eqn{C_p}, go to
#'  \url{https://www.waterontheweb.org/under/waterquality/oxygen.html}.
#'
#'  To determine what value to use for the \code{pressure} argument:
#'
#'  \enumerate{
#'
#'  \item Calculate water pressure: Navigate to:
#'  \url{https://bluerobotics.com/learn/pressure-depth-calculator/?waterType=fresh}.
#'   Choose "Freshwater" or "Saltwater" and use the calculator to determine the
#'  pressure of water above the DO sensor in atm.
#'
#'  \item Calculate air pressure:
#'
#'  If the body of water is at sea level, air pressure is 1 atm.
#'
#'  If the body of water is not at sea level, first determine the altitude from:
#'  \url{https://www.mapcoordinates.net/en}. Next, determine the air pressure in
#'  atm from \url{https://www.mide.com/air-pressure-at-altitude-calculator}
#'  (using the "Calculate Air Pressure at Altitude" calculator).
#'
#'  \item Total pressure = water pressure + air pressure. Use the total pressure
#'  value as the \code{pressure} argument. }
#'
#'@param  dat.tidy Data in tidy format, as returned by the function
#'  \code{convert_to_tidydata()}. Must include three columns: \code{SENSOR}
#'  (character, must include "aquaMeasure-xxxxxx"), \code{VARIABLE} (character,
#'  must include "Temperature" and "Dissolved Oxygen"), and \code{VALUE}
#'  (numeric). Temperature values must be in degrees Celsius, and Dissolved
#'  Oxygen values must be in \% saturation. Other columns will be ignored.
#'@param pressure Total pressure (water pressure + air pressure) in atm on the
#'  DO sensor. See Details section for how to determine an appropriate value.
#'@return Returns \code{dat.tidy} with additional observations in the
#'  \code{VARIABLE} column for "DO_concentration" and the corresponding value in
#'  the \code{VALUE} column.
#'@family calculate
#'@author Danielle Dempsey, Nicole Torrie
#'@importFrom tidyr pivot_wider pivot_longer separate
#'@import dplyr
#'@export
#'
#'

calculate_DO_concentration <- function(dat.tidy, pressure){

  dat <- dat.tidy %>%
    separate(SENSOR, into = c("SENSOR_NAME", NA), sep = "-", remove = FALSE) %>%
    filter(SENSOR_NAME == "aquaMeasure") %>%
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
    mutate(DO_concentration = (cp * `Dissolved Oxygen`)/100) #%>%
   select(-SENSOR_NAME, -cp) %>%
   pivot_longer(cols= c("Temperature", "Dissolved Oxygen", "DO_concentration"),
   names_to = "VARIABLE", values_to = "VALUE")


}


