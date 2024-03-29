% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers_misc.R
\name{rbind_csv_files}
\alias{rbind_csv_files}
\title{Binds together all csv files in a folder}
\usage{
rbind_csv_files(path)
}
\arguments{
\item{path}{Path to the folder with the csv files to bind.}
}
\value{
Returns a dataframe
}
\description{
Binds together all csv files in a folder
}
\details{
All files must have the same column names.
}
