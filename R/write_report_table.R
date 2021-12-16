#' @title Writes deployment table for county report
#' @details This is a temporary function to help prepare OpenData reports.

#' @param dat.tidy Tidy data as formatted by \code{format_for_opendata()}.

#' @param keep.waterbody Logical value indicating whether to keep the
#'  \code{Waterbody} column.
#'
#' @param keep.units Logical value indicating whether to keep the
#'  \code{Units} column.

#' @family OpenData
#' @author Danielle Dempsey

#' @importFrom tidyr separate
#' @import dplyr
#' @export

write_report_table <- function(dat.tidy, keep.waterbody = FALSE, keep.units = FALSE){

  table.out <- dat.tidy %>%
    select(Waterbody = WATERBODY,
           Station = STATION,
           DEPLOYMENT_PERIOD,
           Latitude = LATITUDE,
           Longitude = LONGITUDE,
           Variable = VARIABLE,
           DEPTH,
           Units = UNITS,
           Mooring = MOORING) %>%
    distinct() %>%
    convert_depth_to_ordered_factor() %>%
    rename(`Depth (m)` = DEPTH) %>%
    separate(col = DEPLOYMENT_PERIOD, into = c("Deployment Date", "Retrieval Date"), sep = " to ")

  if(keep.waterbody) table.out <- table.out %>% arrange(Waterbody, Station, `Deployment Date`, `Depth (m)`)

  if(!keep.waterbody) {
    table.out <- table.out %>%
      arrange(Station, `Deployment Date`, `Depth (m)`) %>%
      select(-Waterbody)
  }

  if(!keep.units) table.out <- table.out %>% select(-Units)

  table.out

}
