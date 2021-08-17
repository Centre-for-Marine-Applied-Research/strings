#' @title Calculates dissolved oxygen percent saturation (\%)

#' @description Converts dissolved oxygen from concentration (mg/L) to percent
#'   saturation based on temperature, salinity, and barometric pressure.
#'
#' @details DO percent saturation is calculated as:
#'
#'   \deqn{DO_{\% Saturation} = 100 * DO_{mg/L} / C_{p}}
#'
#'   Where \eqn{DO_{mg/L}} is the concentration of dissolved oxygen in mg/L
#'   (corrected for salinity and/or pressure), and \eqn{C_{p}} is the solubility
#'   of oxygen at the observed temperature, pressure, and salinity.
#'
#'   See \code{?calculate_cp} for information on how \eqn{C_{p}} is calculated.
#'
#' @inheritParams calculate_cp
#'
#' @param dat.wide Dataframe with columns \code{Temperature} (degrees Celsius),
#'   and \code{DO_concentration} (mg/L). Other columns will be ignored and
#'   returned.
#'
#' @param return.factors Logical parameter. If \code{TRUE} the function returns
#'   \code{dat.wide} with additional columns for the parameters used to
#'   calculate \code{C_p} (DO solubility), and  \code{DO_percent_sat}.
#'
#'   If \code{FALSE} (the default), the function returns \code{dat.wide} with
#'   additional column \code{DO_percent_sat} instead of \code{DO_concentration}.
#'
#' @family Dissolved Oxygen
#'
#' @author Danielle Dempsey, Nicole Torrie
#'
#' @importFrom dplyr mutate select
#'
#' @export

calculate_DO_percent_saturation <- function(dat.wide,
                                            Sal = NULL,
                                            P_atm = NULL,
                                            method = "garcia-gordon",
                                            return.factors = FALSE){

  dat.out <- dat.wide %>%
    mutate(Temperature = as.numeric(Temperature),
           DO_concentration = as.numeric(DO_concentration)) %>%
    calculate_cp(Sal = Sal, P_atm = P_atm, return.factors = TRUE) %>%
    mutate(DO_percent_sat = 100 * DO_concentration / C_p)

  if(isFALSE(return.factors)){

    dat.out <- dat.out %>%
      select(-DO_concentration, -C_star, -Salinity, -F_s, -Pressure,
             -T_Kelvin, -theta, -P_wv, -alt_correction, -F_p, -C_p)

  }

  dat.out

}


