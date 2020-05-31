# Need this so that package will play nice with dplyr package
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# more technical solution here: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html


utils::globalVariables(c(
  # compile_HOBO_data()
  "SERIAL",
  "VAR_DEPTH",
  "DATE",
  "INDEX",

  # compile_aquaMeasure_data()
  "Timestamp(UTC)",
  "Record Type",
  "PLACEHOLDER",

  # convert_to_tidydata
  "DEPTH",
  "VALUE",

  # plot_temp_DO_sal
  "VARIABLE"
))

# to add packages to DESCRIPTION:
#usethis::use_package("packageName")
