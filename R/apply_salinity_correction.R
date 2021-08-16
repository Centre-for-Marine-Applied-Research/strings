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
#' @inheritParams DO_salinity_correction
#'
#' @param dat.wide Dataframe with at least two columns: \code{Temperature} and
#'   \code{Dissolved Oxygen}. Corresponding salinity (psu) data may be included
#'   in column \code{Salinity}.
#'
#' @param return.Fs Logical parameter. If \code{TRUE} the function returns a
#'   dataframe with columns of uncorrected DO values, the parameters used to
#'   calculate \code{F_s} (salinity correction factor), and \code{DO_corrected}.
#'
#'   If \code{FALSE}, the function returns \code{dat.wide} with additional
#'   column \code{DO_corrected}.
#'
#' @export


apply_salinity_correction <- function(dat.wide,
                                      Sal = NULL,
                                      method = "garcia-gordon",
                                      return.Fs = FALSE){


  dat.out <- dat.wide %>%
    DO_salinity_correction(Sal = Sal, method = method) %>%
    mutate(
      `Dissolved Oxygen` = as.numeric(`Dissolved Oxygen`),
      DO_corrected = `Dissolved Oxygen` * F_s
    )

  if(isFALSE(return.Fs)){

    dat.out <- dat.out %>%
      select(-`Dissolved Oxygen`, -Salinity, -F_s)

  }

  print("salinity correction factor applied to dissolved oxygen data")
  dat.out

}















