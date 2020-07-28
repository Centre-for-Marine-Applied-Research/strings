# Need this so that package will play nice with dplyr package
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# more technical solution here: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

# other technical solution here:
# https://dplyr.tidyverse.org/articles/programming.html

utils::globalVariables(c(
  # compile_HOBO_data()
  "SERIAL",
  "DATE",
  "INDEX",
  "TEMPERATURE",

  # compile_aquaMeasure_data()
  "Timestamp(UTC)",
  "Record Type",
  "PLACEHOLDER",
  "DATE_VALUES",
  "Dissolved Oxygen",
  "Temperature",

  # compile_vemco_data()
  "Description",
  "Date and Time (UTC)",
  "Data",

  # convert_to_tidydata
  "DEPTH",
  "VALUE",
  "DATE_RANGE",

  # plot_temp_DO_sal
  "VARIABLE",

  # convert_HOBO_datetime_to_real_UTC
  "ADT_force",
  "DAYLIGHT_SAVINGS",
  "UTC",

  # calculate_DO_concentration
  "cp",
  "SENSOR_NAME",

  # dates_to_fix
  "inx_fix",
  "n_fix",

  # read_deployment_log
  "Logger_Model",
  "Serial#",
  "Sensor_Depth",
  "sensor",
  "SENSOR",
  "numeric_depth",

  # trim_data
  "SENSOR_TYPE",

  # format_for_opendata
  "lat",
  "long",
  "COUNTY",

  # calculate_degree_days
  "n_DAYS",
  "AVG_TEMPERATURE",
  "START_DAY",
  "END_DAY",
  "PERIOD"

))

# to add packages to DESCRIPTION:
#usethis::use_package("packageName")
