#' Example data in wide format, as exported from compile_all_data()
#' @format This example data is a dataframe with 3409 rows and 8 columns:
#'   Columns alternate between the timestamp (in the format "Y-m-d H:M:S") and
#'   the variable value (rounded to three decimal places). Metadata at the top
#'   of each column indicates the deployment dates, the sensor serial number,
#'   and the variable and depth of the sensor. Each datetime column shows the
#'   timezone as extracted from the sensor. 'To include the metadata, all values
#'   were converted to class character. To manipulate the data, the values must
#'   be converted to the appropriate class (e.g., POSIXct for the datetimes and
#'   numeric for variable values). This can be done using the function
#'   convert_to_tidydata().

#' @source Data from Coastal Monitoring Program
"wide_data"
