% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assemble_opendata_submission.R
\name{assemble_opendata_submission}
\alias{assemble_opendata_submission}
\title{Assemble sensor string data from different deployments into a csv
 and/or rds file}
\usage{
assemble_opendata_submission(
  input_path = NULL,
  output_path = NULL,
  county = "",
  export_csv = TRUE,
  export_rds = TRUE,
  return_global = FALSE
)
}
\arguments{
\item{input_path}{Path to the *.csv files to be assembled. Default is the Open
Data/Deployment Data folder on the CMAR Operations shared drive (user must
be connected to the Perennia VPN).}

\item{output_path}{Path to where assembled file(s) should be exported. The
default will export the csv file to the Open Data/Submissions folder on the
CMAR Operations shared drive (for submission to the Open Data Portal) and
the rds file to the Open Data/County Datasets folder (for import ). For
custom \code{out_path} entries, the csv and rds files will be exported to
the same folder.}

\item{county}{A character string. If \code{export_csv} and/or
\code{export_rds} is \code{TRUE}, \code{county} is used to name the exported
file(s). Additionally, if \code{input_path = NULL}, \code{county} is the
next folder on the path.}

\item{export_csv}{Logical argument indicating whether the assembled data
should be exported as a *.csv file. File name will be
county_todays-date.csv. Default is \code{TRUE}. Note: \code{TIMESTAMP} is
converted to a character before exporting to remove UTC formatting
(2018-12-23T05:00:00Z). When re-imported into R, the UTC timezone should be
added using the \code{force_tz()} function from the lubridate package.}

\item{export_rds}{Logical argument indicating whether the assembled data
should be exported as a *.rds file. File name will be
county_todays-date.rds. Default is \code{TRUE}.}

\item{return_global}{Logical argument indicating whether the assembled data
should be returned to the global environment. Default is \code{FALSE}.}
}
\value{
Returns the assembled data as a .csv, and/or .rds, and/or as a
 dataframe to the global environment.
}
\description{
Only users connected to the CMAR Operations shared drive can use
 the default \code{input_path} and \code{output_path}.The csv and rds files
 are exported to different folders on the shared drive.
}
\details{
Right now can only handle files as exported from
 \code{format_for_opendata()}. Future versions will also be able to assemble
 data from \code{convert_to_tidydata()}.


 This function gives the option to export as a csv file and/or an rds file.
 For the CMAR work flow, the csv file is for submission to the Open Data
 Portal and the rds file should be used for the county report and other R
 analyses (it is much faster to import the rds file than the csv file).

 Note: data.table::fread and data.table::fwrite were investigated as a faster
 alternative to reading and writing rds files. The data.table functions were
 faster for working with files on the Desktop, but were slower for working
 from the shared drive. This was tested by reading in two csv files (293,318
 KB and 72,408 KB) and the same data in two rds files (9,620 KB and 2,363) 5
 times. data.table::fread took an average of 78 seconds to read in the two
 files, while readRDS() took an average of 9.9 seconds (~8 times faster).
 (readr::read_csv took an average of 105 seconds).

 50,000 rows of this dataset were exported 5 times using data.table::fwrite
 and saveRDS. data.table::fwrite took an average of 12.9 seconds, while
 saveRDS took an average of 10.5 seconds (1.2 times faster).
 (readr::write_csv took an average of 350 seconds).
}
\seealso{
Other OpenData CMAR: 
\code{\link{import_strings_data}()}
}
\author{
Danielle Dempsey
}
\concept{OpenData CMAR}
