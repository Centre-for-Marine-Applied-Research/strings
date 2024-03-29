% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_mooring_column.R
\name{add_mooring_column}
\alias{add_mooring_column}
\title{Add MOORING column to tidydata for submission to Open Data Portal}
\usage{
add_mooring_column(dat, path_mooring = NULL)
}
\arguments{
\item{dat}{Dataframe, as returned from \code{assemble_county_data()}. Must
include columns \code{WATERBODY},\code{STATION}, and
\code{DEPLOYMENT_PERIOD}.}

\item{path_mooring}{Path to the mooring type table (including file name and
extension).}
}
\value{
Returns \code{dat} with additional column \code{MOORING}.
}
\description{
Add MOORING column to tidydata for submission to Open Data Portal
}
