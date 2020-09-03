## code to prepare `deployment_log` dataset goes here

# libraries
library(dplyr)
library(readxl)   # to readcsv file
#library(strings) # for string data functions
#ibrary(viridis) # because 7 depths

# Raw data ----------------------------------------------------------------

# import raw data
deployment_log <- read_excel("C:/Users/Danielle Dempsey/Desktop/RProjects/strings/data-raw/Log/Birchy Head 2019-05-02 Log.xls") %>%
  select(Deployment_Waterbody,
         Location_Description,
         `Lease#`,
         Status, # maybe get rid of this column
         Deployment, Retrieval,
         Logger_Latitude,
         Logger_Longitude,
         Logger_Model,
         `Serial#`,
         Sensor_Depth)



usethis::use_data(deployment_log, overwrite = TRUE)


