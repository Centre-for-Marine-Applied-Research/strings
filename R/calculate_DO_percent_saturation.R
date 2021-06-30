#' @title Calculates dissolved oxygen percent saturation (\%)

#' @description Converts dissolved oxygen from concentration (mg/L) to percent
#'   saturation based on temperature, salinity, and barometric pressure.
#'
#' @details DO percent saturation is calculated as:
#'
#'   \deqn{DO_{\% Saturation} = 100 * DO_{mg/L} / C_{p}}
#'
#'   DO_{mg/L} is the concentration of dissolved oxygen in mg/L (make sure the
#'   salinity and/or pressure-corrected concentration is used if required).
#'
#'   C_p is the solubility of oxygen at the observed temperature, pressure, and
#'   salinity.
#'
#'   C_p is calculated using equations 24 and 32, and Table 2 from Benson and
#'   Krause 1984.
#'
#'   Benson, Bruce B., Krause, Daniel, (1984), The concentration and isotopic
#'   fractionation of oxygen dissolved in freshwater and seawater in equilibrium
#'   with the atmosphere, Limnology and Oceanography, 3, doi:
#'   10.4319/lo.1984.29.3.0620.
#'
#' @inheritParams calculate_cp
#'
#' @param dat.wide Dataframe with columns \code{TIMESTAMP}, \code{Temperature}
#'   (degrees Celsius), and \code{DO_concentration} (mg/L). Other columns will
#'   be ignored.
#'
#' @param return.cp Logical parameter. If \code{TRUE}, the function returns a
#'   wide dataframe with columns: \code{TIMESTAMP}, \code{Temperature},
#'   \code{DO_concentraion} (in mg/L), the parameters used to calculate
#'   \code{C_p}, \code{C_p}, and \code{DO_percent_sat}.
#'
#'   If \code{FALSE}, the function returns a wide dataframe with columns:
#'   \code{TIMESTAMP}, \code{Temperature}, \code{DO_percent_sat} (dissolved
#'   oxygen in units of percent saturation).
#'
#' @family calculate
#'
#' @author Danielle Dempsey, Nicole Torrie
#'
#' @importFrom dplyr mutate
#'
#' @export

calculate_DO_percent_saturation <- function(dat.wide,
                                            Sal = NULL,
                                            P_atm = NULL,
                                            return.cp = FALSE){

  dat.out <- dat.wide %>%
    mutate(Temperature = as.numeric(Temperature),
           DO_concentration = as.numeric(DO_concentration)) %>%
    calculate_cp(Sal = Sal, P_atm = P_atm) %>%
    mutate(DO_percent_sat = 100 * DO_concentration / C_p)

  if(isFALSE(return.cp)){

    dat.out <- dat.out %>%
      select(-DO_concentration, -Salinity, -Pressure,
             -T_Kelvin, -theta, -P_wv, -C_star, -alt_correction, -C_p)

  }

  dat.out

}


