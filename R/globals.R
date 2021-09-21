# Need this so that package will play nice with dplyr package
# https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887
# more technical solution here: https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

# other technical solution here:
# https://dplyr.tidyverse.org/articles/programming.html

utils::globalVariables(c(
  # compile_HOBO_data()
  "SERIAL",
  "TIMESTAMP",
  "INDEX",
  "TEMPERATURE",
  "COL_NAME",
  "sensor.depth",
  "DO_corrected",

  # compile_aquaMeasure_data()
  "Timestamp(UTC)",
  "Record Type",
  "PLACEHOLDER",
  "DATE_VALUES",
  "Dissolved Oxygen",
  "Temperature",
  "Temp(Water)",

  # compile_vemco_data()
  "Description",
  "Date and Time (UTC)",
  "Date/Time",
  "Data",

  # convert_to_tidydata
  "DEPTH",
  "VALUE",
  "DEPLOYMENT_PERIOD",

  # plot_temp_DO_sal
  "VARIABLE",

  # convert_HOBO_datetime_to_real_UTC
  "ADT_force",
  "DAYLIGHT_SAVINGS",
  "UTC",

  # calculate_DO_concentration
  "cp",
  "SENSOR_NAME",

  # calculate_cp
  "T_Kelvin",
  "C_star",
  "PP_wv",
  "theta",
  "C_p",
  "Salinity",
  "P_wv",
  "Pressure",
  "alt_correction",

  # DO_salinity_correction
  "T_s",
  "F_s",

  # DO_pressure_correction
  "F_p",

  # dates_to_fix
  "inx_fix",
  "n_fix",

  # read_deployment_log
  "Logger_Model",
  "Logger_Lower",
  "detect_am",
  "detect_hobo",
  "detect_tidbit",
  "detect_vemco",
  "Serial#",
  "Sensor_Depth",
  "sensor",
  "SENSOR",
  "numeric_depth",

  # trim_data
  "SENSOR_TYPE",

  # calculate_DO_percent_calculation
  "DO_concentration",
  "DO_percent_sat",

  # format_for_opendata
  "LATITUDE", "LEASE", "LONGITUDE", "STATION", "TIMESTAMP", "WATERBODY",

  # name_for_open_data
  "START_DAY",

  # write_report_table
  "Deployment Date",
  "Depth (m)",
  "Station",

  # convert_depth_to_ordered_factor
  "DEPTH_QUAL",

  # import_strings_data
  "County",
  "COUNTY",

  # create_deployment_log & combine_deployment_logs
  "Acoustic_Release?",
  "Anchor_Wgt",
  "Comments",
  "Depl_Attendant",
  "Depl_Date",
  "Depl_Duration",
  "Depl_Lat" ,
  "Depl_Lon" ,
  "Depl_Sounding" ,
  "Depl_Time" ,
  "Depl_Voltage",
  "Deployment" ,
  "Deployment_Waterbody" ,
  "Duration",
  "Inst_Depth",
  "Inst_Model",
  "Inst_Serial",

  "Lease#",
  "Location_Description" ,
  "Logger_Latitude",
  "Logger_Longitude",
  "Notes" ,
  "Recv_Attendant" ,
  "Recv_Date" ,
  "Recv_Lat" ,
  "Recv_Lon",
  "Recv_Method" ,
  "Recv_Voltage" ,
  "Retrieval" ,
  "Sounding" ,
  "Station_Name" ,
  "Status",
  "Waterbody",
   "Location_lower"



))

# to add packages to DESCRIPTION:
#usethis::use_package("packageName")
