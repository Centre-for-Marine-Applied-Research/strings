#' Apply salinity correction factor to dissolved oxygen measurements
#'
#' @details Dissolved oxygen **needs to be corrected
#'
#' Equation
#'
#'
#'
#'    calculated using the
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
#' @param dat.wide Dataframe with at least two columns: \code{Temperature} and
#'   \code{Dissolved Oxygen}. Corresponding salinity (psu) data may be included
#'   in column \code{Salinity}.
#'
#' @param Sal A single value of salinity (psu). This value must be specified if
#'   there is no \code{Salinity} column in \code{dat.wide}. Default is \code{Sal
#'   = NULL}. Note: if \code{Sal} is specified when there is a \code{Salinity}
#'   column in \code{dat.wide}, the function will stop with an error.
#'
#' @param return.Fs Logical parameter. If \code{TRUE} the function returns a
#'   dataframe with columns: \code{TIMESTAMP}, \code{Temperature},
#'   \code{Dissolved Oxygen} (uncorrected values), the parameters used to
#'   calculate \code{Salinity}, \code{T_Kelvin} (temperature in Kelvin),
#'   \code{F_s} (salinity correction factor), and\code{DO_corrected}.
#'
#'   If \code{FALSE}, the function returns a wide dataframe with columns:
#'   \code{TIMESTAMP}, \code{Temperature}, \code{DO_corrected}.
#'
#' @export


apply_salinity_correction <- function(dat.wide, Sal = NULL, return.Fs = FALSE){

  cols <- names(dat.wide)

  if("Salinity" %in% cols & !is.null(Sal)){

    stop("Conflicting salinity values.
         \nHINT: Remove column Salinity or set Sal argument to NULL.")

  }

  if(!is.null(Sal)){
    dat.wide <- dat.wide %>%
      mutate(Salinity = Sal)
  }

  dat.out <- dat.wide %>%
    mutate(
      Temperature = as.numeric(Temperature),
      `Dissolved Oxygen` = as.numeric(`Dissolved Oxygen`),

      # temperature in Kelvin
      T_Kelvin = Temperature + 273.15,

      # correction factor
      F_s = exp(-Salinity * (0.017674 -
                               10.754 / T_Kelvin + 2140.7 / T_Kelvin^2)),

      # corrected DO
      DO_corrected = `Dissolved Oxygen` * F_s
    )

  if(isFALSE(return.Fs)){

    dat.out <- dat.out %>%
      select(-`Dissolved Oxygen`, -Salinity, -T_Kelvin, -F_s)

  }

  dat.out

}















