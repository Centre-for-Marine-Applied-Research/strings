#' Calculate the solubility of dissolved oxygen
#'
#' @description Calculate the solubility of dissolved oxygen based on
#'   temperature, salinity and barometric pressure, following the equations in
#'   Benson and Krause 1984.
#'
#' @details Solubility of dissolved oxygen (mg/L) is calculated using the
#'   equations in Benson and Krause 1984.
#'
#'   These equations should only be used when temperature is between 0 and 40
#'   degrees-Celsius and salinity is between 0 and 40 psu.
#'
#'   Results from this function should match those from the USGS DOTABLES at
#'   \url{https://water.usgs.gov/water-resources/software/DOTABLES/}.
#'
#'   For more info see equations 24 and 32, and Table 2 from Benson and Krause
#'   1984.
#'
#'   Benson, Bruce B., Krause, Daniel, (1984), The concentration and isotopic
#'   fractionation of oxygen dissolved in freshwater and seawater in equilibrium
#'   with the atmosphere, Limnology and Oceanography, 3, doi:
#'   10.4319/lo.1984.29.3.0620.
#'
#' @param dat.wide Dataframe with at least one column: \code{Temperature}.
#'   Corresponding salinity (psu) and pressure (atm) data may be included in
#'   columns \code{Salinity} and \code{Pressure}.
#'
#' @param Sal A single value of salinity (ppt). This value should be specified
#'   if there is no \code{Salinity} column in \code{dat.wide}. Default is
#'   \code{Sal = NULL}. Note: if \code{Sal} is specified when there is a
#'   \code{Salinity} column in \code{dat.wide}, function will stop with an
#'   error.
#'
#' @param P_atm A single value of barometric pressure (atm). This value should
#'   be specified if there is no \code{Pressure} column in \code{dat.wide}.
#'   Default is \code{P_atm = NULL}. Note: if \code{P_atm} is specified when
#'   there is a \code{Pressure} column in \code{dat.wide}, function will stop
#'   with an error.
#'
#' @export



calculate_cp <- function(dat.wide, Sal = NULL, P_atm = NULL){

  cols <- names(dat.wide)

  if("Salinity" %in% cols & !is.null(Sal)){

    stop("Conflicting salinity values.
         \nHINT: Remove column Salinity or set Sal argument to NULL.")

  }

  if("Pressure" %in% cols & !is.null(P_atm)){

    stop("Conflicting pressure values.
         \nHINT: Remove column Pressure or set P_atm argument to NULL.")

  }

  if(!is.null(Sal)){
    dat.wide <- dat.wide %>%
      mutate(Salinity = Sal)
  }

  if(!is.null(P_atm)){
    dat.wide <- dat.wide %>%
      mutate(Pressure = P_atm)
  }


  dat.wide %>%
    mutate(
      Temperature = as.numeric(Temperature),

      # temperature in Kelvin
      T_Kelvin = Temperature + 273.15,

      # Coefficient that depends on the Second Virial Coefficient of oxygen
      # Table 2
      theta = 0.000975 - (1.426e-5) * Temperature + (6.436e-8) * Temperature^2,

      # partial pressure of water vapour in atm
      # Table 2
      P_wv = (1 - 5.370e-4 * Salinity) *
        exp(
          18.1973 * (1 - 373.16 / T_Kelvin) +
            3.1813e-7 * (1 - exp(26.1205 * (1 - T_Kelvin / 373.16))) -
            1.8726e-2 * (1 - exp(8.03945 * (1 - 373.16 / T_Kelvin))) +
            5.02802 * log(373.16 / T_Kelvin)
        ),

      # Unit standard atmospheric concentration (mg / L)
      # Equation 32 (modified to return units of mg / L)
      C_star = exp(
        -139.34411 +
          1.575701e5 / T_Kelvin -
          6.642308e7 / T_Kelvin^2 +
          1.243800e10 / T_Kelvin^3 -
          8.621949e11 / T_Kelvin^4 -
          # this term is F_s in apply_salinity_correction()
          Salinity * (0.017674 - 10.754 / T_Kelvin + 2140.7 / T_Kelvin^2)
      ),

      # square brackets term in Equation 24
      alt_correction = ((1 - P_wv / Pressure) * (1 - theta * Pressure)) /
        ((1 - P_wv) * (1 - theta)),

      # Equation 24
      C_p = C_star * Pressure * alt_correction

    )

}
