#'@title Writes deployment table for county report
#'@details This is a temporary function to help prepare OpenData reports.

#'@param dat.tidy Tidy data as formatted by \code{format_for_opendata()}.
#'@param keep.waterbody Logical value indicating whether to keep the
#'  \code{Waterbody} column.
#'@family OpenData
#'@author Danielle Dempsey

#'@importFrom tidyr separate
#'@import dplyr
#'@export

write_report_table <- function(dat.tidy, keep.waterbody = FALSE){

  table.out <- dat.tidy %>%
    select(Waterbody = WATERBODY,
           Station = STATION,
           DEPLOYMENT_PERIOD,
           Latitude = LATITUDE,
           Longitude = LONGITUDE,
           `Sensor Type` = VARIABLE,
           DEPTH) %>%
    distinct() %>%
    convert_depth_to_ordered_factor() %>%
    rename(`Depth (m)` = DEPTH) %>%
    separate(col = DEPLOYMENT_PERIOD, into = c("Deployment Date", "Retrieval Date"), sep = " to ")

    #
    # mutate(DEPTH = if_else(!is.na(suppressWarnings(as.numeric(DEPTH))),
    #                              as.numeric(DEPTH), DEPTH)) %>%
    # rename(`Depth (m)` = DEPTH) %>%
    # separate(col = DEPLOYMENT_PERIOD, into = c("Deployment Date", "Retrieval Date"), sep = " to ")

  # if(!is.na(suppressWarnings(as.numeric(table.out$`Depth (m)`[1])))){
  #   table.out <- table.out  %>%
  #     mutate(`Depth (m)` = as.numeric(`Depth (m)`))
  # }

  if(keep.waterbody == TRUE) table.out <- table.out %>% arrange(Waterbody, Station, `Deployment Date`, `Depth (m)`)

  if(keep.waterbody == FALSE) table.out <- table.out %>% arrange(Station, `Deployment Date`, `Depth (m)`) %>% select(-Waterbody)

  table.out

}
