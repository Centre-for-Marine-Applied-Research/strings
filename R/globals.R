# Need this so that package will play nice with dplyr package
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# more technical solution here: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html


utils::globalVariables(c(
  # compile_HOBO_data()
  "SERIAL",
  "VAR_DEPTH",
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

  # convert_to_tidydata
  "DEPTH",
  "VALUE",

  # plot_temp_DO_sal
  "VARIABLE",

  # convert_HOBO_datetime_to_real_UTC
  "ADT_force",
  "DAYLIGHT_SAVINGS",
  "UTC",

  # dates_to_fix
  "inx_fix",
  "n_fix"


))

# to add packages to DESCRIPTION:
#usethis::use_package("packageName")
