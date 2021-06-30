#' Apply salinity correction factor to dissolved oxygen measurements
#'
#' @details Dissolved oxygen measured by HOBO sensors needs to be corrected for
#'   salinity (see manual:
#'   \url{https://www.onsetcomp.com/files/manual_pdfs/15603-B-MAN-U26x.pdf}).
#'
#'   The salinity correction factor is calculated from the final term in
#'   equation 32 in Benson and Krause 1984:
#'
#'   \deqn{F_{s} = exp(-Salinity * (0.017674 - 10.754 / T_{Kelvin} + 2140.7 /
#'   T_{Kelvin}^2))}
#'
#'   T_{Kelvin} is the temperature in Kelvin and \eqn{Salinity} is the salinity
#'   in ppt.
#'
#'   Results from this function should match those from the USGS DOTABLES (part
#'   C) at \url{https://water.usgs.gov/water-resources/software/DOTABLES/}.
#'
#'   This equation should only be used when temperature is between 0 and 40
#'   degrees-Celsius and salinity is between 0 and 40 psu.
#'
#'   Note: the HOBO Dissolved Oxygen Assistant Software uses a different
#'   equation to calculate the correction factor, but the results for a given
#'   temperature and salinity are typically the same as the Benson and Krause
#'   equation (until the fourth decimal place).
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

  print("salinity correction factor applied to dissolved oxygen data")
  dat.out

}















