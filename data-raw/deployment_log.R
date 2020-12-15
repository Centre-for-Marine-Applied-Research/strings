## code to prepare `deployment_log` dataset goes here

# libraries
library(dplyr)
library(readxl)   # to readcsv file

# Raw data ----------------------------------------------------------------

# import raw data
deployment_log <- read_excel("C:/Users/Danielle Dempsey/Desktop/RProjects/strings/data-raw/Log/Borgles Island 2019-05-30 Log.xls") %>%
  select(Deployment_Waterbody,
         Location_Description,
         `Lease#`,
         Deployment, Retrieval,
         Logger_Latitude,
         Logger_Longitude,
         Logger_Model,
         `Serial#`,
         Sensor_Depth)

usethis::use_data(deployment_log, overwrite = TRUE)


