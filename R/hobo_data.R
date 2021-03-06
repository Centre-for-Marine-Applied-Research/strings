#' Example of compiled Hobo data in wide format, as exported from
#' compile_HOBO_data()
#' @format This example data is a dataframe with 19585 rows and 7 columns,
#'   compiled from three Hobo sensors. The first column is an index column
#'   (starting at -4 to account for the metadata rows). The remaining columns
#'   alternate between the timestamp (in the format "Y-m-d H:M:S") and the
#'   variable value (rounded to three decimal places). Metadata at the top of
#'   each column indicates the deployment and retrieval dates, the sensor serial
#'   number, and the variable and depth of the sensor. Each timestamp column
#'   shows the timezone as extracted from the sensor. To include the metadata,
#'   all values were converted to class character. To manipulate the data, the
#'   values must be converted to the appropriate class (e.g., POSIXct for the
#'   datetimes and numeric for variable values). This can be done using the
#'   function convert_to_tidydata().

#' @source Data from Coastal Monitoring Program
"hobo_data"
