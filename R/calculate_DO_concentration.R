#' @title Calculates dissolved oxygen concentration (mg/L)
#'
#' @description Converts dissolved oxygen from percent saturation to
#'   concentration (mg/L) based on temperature, salinity, and barometric
#'   pressure.
#'
#' @details DO concentration is calculated as:
#'
#'   \deqn{DO_{mg/L} = C_{p} * DO_{\% Saturation} /100 }
#'
#'   Where \eqn{DO_{\% Saturation}} is the percent saturation of dissolved
#'   oxygen, and \eqn{C_{p}} is the solubility of oxygen at the observed
#'   temperature, pressure, and salinity.
#'
#'   See \code{?calculate_cp} for information on how \eqn{C_{p}} is calculated.
#'
#' @inheritParams calculate_DO_percent_saturation
#'
#' @param dat.wide Dataframe with columns \code{Temperature} (degrees Celsius),
#'   and \code{DO_percent_sat} (\% saturation). Other columns will be ignored
#'   and returned.
#'
#' @param return.factors Logical parameter. If \code{TRUE} the function returns
#'   \code{dat.wide} with additional columns for the parameters used to
#'   calculate \code{C_p} (DO solubility), and  \code{DO_concentraion} (in
#'   mg/L).
#'
#'   If \code{FALSE} (the default), the function returns \code{dat.wide} with
#'   additional column \code{DO_concentraion} (in mg/L) instead of
#'   \code{DO_percent_sat}.
#'
#' @family Dissolved Oxygen
#'
#' @author Danielle Dempsey, Nicole Torrie
#'
#' @importFrom dplyr mutate select
#'
#' @export
#'

calculate_DO_concentration <- function(dat.wide,
                                       Sal = NULL,
                                       P_atm = NULL,
                                       method = "garcia-gordon",
                                       return.factors = FALSE){

  dat.out <- dat.wide %>%
    mutate(Temperature = as.numeric(Temperature),
           DO_percent_sat = as.numeric(DO_percent_sat)) %>%
    calculate_cp(Sal = Sal, P_atm = P_atm, return.factors = TRUE) %>%
    mutate(DO_concentration = (C_p * DO_percent_sat)/100)


  if(isFALSE(return.factors)){

    dat.out <- dat.out %>%
      select(-DO_percent_sat, -C_star, -Salinity, -F_s, -Pressure,
             -T_Kelvin, -theta, -P_wv, -alt_correction, -F_p, -C_p)

  }

  dat.out


}


