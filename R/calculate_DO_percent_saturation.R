#' @title Calculates dissolved oxygen percent saturation (\%)
#' @description Converts dissolved oxygen from concentration (mg/L) to percent
#'   saturation based on temperature and pressure.
#'
#' @details DO percent saturation is calculated as:
#'
#'   \deqn{DO_{\% Saturation} = 100 * DO_{mg/L} / C_p}
#'
#'
#'   To see the calculation for \eqn{C_p}, go to
#'   \url{https://www.waterontheweb.org/under/waterquality/oxygen.html}.
#'
#'   To determine what value to use for the \code{pressure} argument:
#'
#'   \enumerate{
#'
#'   \item Calculate water pressure: Navigate to:
#'   \url{https://bluerobotics.com/learn/pressure-depth-calculator/?waterType=fresh}.
#'    Choose "Freshwater" or "Saltwater" and use the calculator to determine the
#'   pressure of water above the DO sensor in atm.
#'
#'   \item Calculate air pressure:
#'
#'   If the body of water is at sea level, air pressure is 1 atm.
#'
#'   If the body of water is not at sea level, first determine the altitude
#'   from: \url{https://www.mapcoordinates.net/en}. Next, determine the air
#'   pressure in atm from
#'   \url{https://www.mide.com/air-pressure-at-altitude-calculator} (using the
#'   "Calculate Air Pressure at Altitude" calculator).
#'
#'   \item Total pressure = water pressure + air pressure. }
#'
#' @param  dat.wide Temperature values must be in degrees Celsius, and Dissolved
#'   Oxygen values must be in mg/L. Other columns will be ignored and not
#'   returned.
#' @param air.pressure Air pressure in atm on the DO sensor. See Details section
#'   for how to determine an appropriate value.
#' @param sensor.depth Depth of the sensor from the surface in metres. Used to
#'   calculate the water pressure on the sensor.
#' @param salt.water Logical parameter. If \code{TRUE} the density of salt water
#'   is used to estimate water pressure. If \code{FALSE} the density of fresh
#'   water is used to estimate water pressure. Default is \code{salt.water =
#'   TRUE}.
#' @param return.cp Logical parameter. If \code{TRUE}, the function returns a
#'   wide dataframe with columns \code{TIMESTAMP}, \code{Temperature},
#'   \code{DO_percent_sat}, \code{DO_concentration}, and \code{cp}. If
#'   \code{FALSE}, the function returns a wide dataframe with columns
#'   \code{TIMESTAMP}, \code{Temperature}, \code{Dissolved Oxygen} (with
#'   dissolved oxygen in units of percent saturation).
#' @family calculate
#' @author Danielle Dempsey, Nicole Torrie
#' @importFrom dplyr mutate
#' @export


calculate_DO_percent_saturation <- function(dat.wide,
                                            air.pressure = 1,
                                            sensor.depth,
                                            salt.water = TRUE,
                                            return.cp = FALSE){


  pressure <- air.pressure

  dat <- dat.wide %>%
    mutate(Temperature = as.numeric(Temperature),
           DO_concentration = as.numeric(Temperature)) %>%
    mutate(
      cp = ((exp(7.7117-1.31403*log(Temperature + 45.93))) *
              pressure *
              (1 - exp(11.8571 - (3840.7 / (Temperature + 273.15))-
                         (216961 / ((Temperature + 273.15)^2))) / pressure) *
              (1 - (0.000975 - (0.00001426 * Temperature) +
                      (0.00000006436 * (Temperature^2))) * pressure)) /
        (1 - exp(11.8571 - (3840.7 / (Temperature+273.15)) -
                   (216961 / ((Temperature + 273.15)^2)))) /
        (1 - (0.000975 - (0.00001426 * Temperature) +
                (0.00000006436 * (Temperature^2))))
    ) %>%
    mutate(DO_percent_sat = 100 * DO_concentration / cp)

  if(isFALSE(return.cp)){

    dat <- dat %>%
      mutate(`Dissolved Oxygen` = DO_percent_sat) %>%
      select(TIMESTAMP, Temperature, `Dissolved Oxygen`)

  }


  dat


}


