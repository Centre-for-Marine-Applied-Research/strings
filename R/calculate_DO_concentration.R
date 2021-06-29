#' @title Calculates dissolved oxygen concentration (mg/L)

#' @description Converts dissolved oxygen from percent saturation to
#'  concentration (mg/L) based on temperature, salinity, and barometric pressure.
#'

#'@details DO concentration is calculated as: Where C_p is the solubility of
#'  oxygen at the observed temperature, pressure, and salinity.
#'
#'  C_p is calculated using equations 24 and 32, and Table 2 from Benson and
#'  Krause 1984.
#'
#'  Benson, Bruce B., Krause, Daniel, (1984), The concentration and isotopic
#'  fractionation of oxygen dissolved in freshwater and seawater in equilibrium
#'  with the atmosphere, Limnology and Oceanography, 3, doi:
#'
#'@inheritParams calculate_DO_percent_saturation
#'
#'@param dat.wide Dataframe with columns \code{TIMESTAMP}, \code{Temperature}
#'  (degrees Celsius), and \code{DO_percent_sat} (\% saturation). Other columns
#'  will be ignored.
#'
#' @param return.cp Logical parameter. If \code{TRUE}, the function returns a
#'   wide dataframe with columns: \code{TIMESTAMP}, \code{Temperature},
#'   \code{DO_concentraion} (in mg/L), the parameters used to calculate
#'   \code{C_p}, \code{C_p}, and \code{DO_percent_sat}.
#'
#'   If \code{FALSE}, the function returns a wide dataframe with columns:
#'   \code{TIMESTAMP}, \code{Temperature}, \code{DO_concentration} (dissolved
#'   oxygen in units of mg/L).
#'
#'
#'@family calculate
#'
#'@author Danielle Dempsey, Nicole Torrie
#'
#'@importFrom tidyr pivot_longer separate
#'
#'@importFrom dplyr mutate
#'
#'@export
#'


calculate_DO_concentration <- function(dat.wide,
                                       Sal = NULL,
                                       P_atm = NULL,
                                       return.cp = FALSE){

  dat.out <- dat.wide %>%
    mutate(Temperature = as.numeric(Temperature),
           DO_percent_sat = as.numeric(DO_percent_sat)) %>%
    calculate_cp() %>%
    mutate(DO_concentration = (C_p * DO_percent_sat)/100)


  if(isFALSE(return.cp)){

    dat.out <- dat.out %>%
      select(-DO_percent_sat, -Salinity, -Pressure,
             -T_Kelvin, -theta, -P_wv, -C_star, -alt_correction, -C_p)

  }

  dat.out


}


