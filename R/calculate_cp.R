#' Calculate the solubility of dissolved oxygen
#'
#' @description Calculate the solubility of dissolved oxygen based on
#'   temperature, salinity and barometric pressure, following the equations in
#'   Benson and Krause 1984 or Garcia and Gordon 1992.
#'
#' @details Solubility of dissolved oxygen (mg/L) is calculated using the
#'   equations in Benson and Krause 1984, or the or Garcia and Gordon 1992
#'   refit. These equations should only be used when 0 < Temperature < 40
#'   degrees Celcius, 0 < Salinity < 40 PSU and 0.5 < Pressue < 1.1 atm.
#'
#'   Results from this function should closely match those from the USGS
#'   DOTABLES (output from GG may differ in the second decimal place)  at
#'   \url{https://water.usgs.gov/water-resources/software/DOTABLES/}.
#'
#'   For more info see equations 24 and 32, and Table 2 from Benson and Krause
#'   1984 or Equation 8 and Table 1 from Garcia and Gordon 1992.
#'
#'   For the Garcia and Gordon equation, coefficients from the first column of
#'   Table 1 were used here. Conversion factor of 1.42905 was used to convert
#'   from cm^3/dm^3 (mL/L) to mg/L (USGS 2011).
#'
#'   Benson, Bruce B., Krause, Daniel, (1984), The concentration and isotopic
#'   fractionation of oxygen dissolved in freshwater and seawater in equilibrium
#'   with the atmosphere, Limnology and Oceanography, 3, doi:
#'   10.4319/lo.1984.29.3.0620.
#'
#'   Garcia, H., and L. Gordon (1992), \emph{Oxygen solubility in seawater:
#'   Better fitting equations}, Limnol. Oceanogr., 37(6).
#'
#'   USGS. \emph{Change to Solubility Equations for Oxygen in Water.} Technical
#'   Memorandum 2011.03. USGS Office of Water Quality, 2011.
#'
#' @inheritParams DO_salinity_correction
#' @inheritParams DO_pressure_correction
#'
#' @param dat.wide Dataframe with at least one column: \code{Temperature}.
#'   Corresponding salinity (psu) and pressure (atm) data may be included in
#'   columns \code{Salinity} and \code{Pressure}.
#'
#' @param return.factors Logical parameter. If \code{TRUE} the function returns
#'   a dataframe with columns of uncorrected DO values, the parameters used to
#'   calculate \code{F_s} and \code{F_p}, and \code{C_p}.
#'
#'   If \code{FALSE}, the function returns \code{dat.wide} with additional
#'   column(s) \code{Salinity}, \code{Pressure}, and \code{C_p}.
#'
#' @export



calculate_cp <- function(dat.wide,
                         Sal = NULL, P_atm = NULL,
                         method = "garcia-gordon",
                         return.factors = FALSE){


# Error messages  ---------------------------------------------------------

  cols <- names(dat.wide)

  if("Salinity" %in% cols & !is.null(Sal)){

    stop("Conflicting salinity values.
         \nHINT: Remove column Salinity or set Sal argument to NULL.")

  }

  if("Pressure" %in% cols & !is.null(P_atm)){

    stop("Conflicting pressure values.
         \nHINT: Remove column Pressure or set P_atm argument to NULL.")

  }

  if(!is.null(Sal)) dat.wide <- mutate(dat.wide, Salinity = Sal)

  if(!is.null(P_atm)) dat.wide <- mutate(dat.wide, Pressure = P_atm)

# Benson Krause ------------------------------------------------------------

  if(tolower(method) == "benson-krause"){

    dat.out <- dat.wide %>%
      mutate(
        Temperature = as.numeric(Temperature),

        # temperature in Kelvin
        T_Kelvin = Temperature + 273.15,

        # Unit standard atmospheric concentration (mg / L)
        # Equation 32 (modified to return units of mg / L)
        C_star = exp(
          -139.34411 +
            1.575701e5 / T_Kelvin -
            6.642308e7 / T_Kelvin^2 +
            1.243800e10 / T_Kelvin^3 -
            8.621949e11 / T_Kelvin^4
          # this term is accounted for in DO_salinity_correction
          # -Salinity * (0.017674 - 10.754 / T_Kelvin + 2140.7 / T_Kelvin^2)
        )
      ) %>%
      DO_salinity_correction(method = "benson-krause") %>%
      DO_pressure_correction() %>%
      mutate(
        C_p = C_star * F_s * F_p
      )

  }


# Garcia Gordon -----------------------------------------------------------

  if(tolower(method) == "garcia-gordon"){

    # coefficients
    A0_GG <- 2.00907
    A1_GG <- 3.22014
    A2_GG <- 4.05010
    A3_GG <- 4.94457
    A4_GG <- -2.56847E-1
    A5_GG <- 3.88767

    B0_GG = -6.24523E-3
    B1_GG = -7.37614E-3
    B2_GG = -1.03410E-2
    B3_GG = -8.17083E-3
    C0_GG = -4.88682E-7

    mg_L <- 1.42905   # to convert from mL/L to mg/L

    dat.out <- dat.wide %>%
      mutate(
        Temperature = as.numeric(Temperature),

        # scaled temperature
        T_s = log((298.15 - Temperature)/(273.15 + Temperature)),

        # oxygen solubility (equation 8, coefficients from first col Table 1)
        C_star = mg_L * exp(
          A0_GG
          + A1_GG * T_s
          + A2_GG * T_s^2
          + A3_GG * T_s^3
          + A4_GG * T_s^4
          + A5_GG * T_s^5
          # these terms are accounted for in DO_salinity_correction
         # + Salinity * (B0_GG + B1_GG * T_s + B2_GG * T_s^2 + B3_GG * T_s^3)
         # + C0_GG * Salinity^2
        )
      ) %>%
      DO_salinity_correction(method = "garcia-gordon") %>%
      DO_pressure_correction() %>%
      mutate(
        C_p = C_star * F_s * F_p
      )

  }

  if(isFALSE(return.factors)){

    dat.out <- dat.out %>%
      select(-C_star, -F_s, -T_Kelvin, -theta, -P_wv, - alt_correction, -F_p)

  }

  dat.out



}
