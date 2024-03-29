% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_DO_percent_saturation.R
\name{calculate_DO_percent_saturation}
\alias{calculate_DO_percent_saturation}
\title{Calculates dissolved oxygen percent saturation (\%)}
\usage{
calculate_DO_percent_saturation(
  dat.wide,
  Sal = NULL,
  P_atm = NULL,
  method = "garcia-gordon",
  return.factors = FALSE
)
}
\arguments{
\item{dat.wide}{Dataframe with columns \code{Temperature} (degrees Celsius),
and \code{DO_concentration} (mg/L). Other columns will be ignored and
returned.}

\item{Sal}{A single value of salinity (psu). This value must be specified if
there is no \code{Salinity} column in \code{dat.wide}. Default is \code{Sal
= NULL}. Note: if \code{Sal} is specified when there is a \code{Salinity}
column in \code{dat.wide}, the function will stop with an error.}

\item{P_atm}{A single value of barometric pressure (atm). This value should
be specified if there is no \code{Pressure} column in \code{dat.wide}.
Default is \code{P_atm = NULL}. Note: if \code{P_atm} is specified when
there is a \code{Pressure} column in \code{dat.wide}, function will stop
with an error.}

\item{method}{Equation to use to calculate dissolved oxygen solubility.
Options are \code{method = "garcia-gordon"} (the default) and \code{method
= "benson-krause"}.}

\item{return.factors}{Logical parameter. If \code{TRUE} the function returns
  \code{dat.wide} with additional columns for the parameters used to
  calculate \code{C_p} (DO solubility), and  \code{DO_percent_sat}.

  If \code{FALSE} (the default), the function returns \code{dat.wide} with
  additional column \code{DO_percent_sat} instead of \code{DO_concentration}.}
}
\description{
Converts dissolved oxygen from concentration (mg/L) to percent
  saturation based on temperature, salinity, and barometric pressure.
}
\details{
DO percent saturation is calculated as:

  \deqn{DO_{\% Saturation} = 100 * DO_{mg/L} / C_{p}}

  Where \eqn{DO_{mg/L}} is the concentration of dissolved oxygen in mg/L
  (corrected for salinity and/or pressure), and \eqn{C_{p}} is the solubility
  of oxygen at the observed temperature, pressure, and salinity.

  See \code{?calculate_cp} for information on how \eqn{C_{p}} is calculated.
}
\seealso{
Other Dissolved Oxygen: 
\code{\link{DO_pressure_correction}()},
\code{\link{DO_salinity_correction}()},
\code{\link{calculate_DO_concentration}()},
\code{\link{calculate_cp}()}
}
\author{
Danielle Dempsey, Nicole Torrie
}
\concept{Dissolved Oxygen}
