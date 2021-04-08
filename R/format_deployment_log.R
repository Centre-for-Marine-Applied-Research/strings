#' @title Create formatted xlsx workbook add log information
#' @details Called by \code{create_log_from_NSDFA_tracking()} and
#'   \code{create_initial_deployment_log()}.

#' @param deployment_log Deployment log created from NSDFA tracking sheet or

#' @return Returns deployment log in .csv format.
#' @author Danielle Dempsey
#' @importFrom magrittr  %T>%
#' @import openxlsx


format_deployment_log <- function(deployment_log){

  body_style <- createStyle(halign = "center", valign = "center", wrapText = TRUE)

  header_style <- createStyle(textDecoration = "bold")

  createWorkbook() %T>%
    addWorksheet(sheetName = "Sheet1")  %T>%
    # set column widths
    setColWidths(sheet = "Sheet1",
                 cols = 1:ncol(deployment_log), widths = 15) %T>%
    # make the comment column width wider
    setColWidths(sheet = "Sheet1", cols = 20, widths = 30)  %T>%
    # set the row height
    setRowHeights(sheet = "Sheet1",
                  rows = 1:(nrow(deployment_log) + 1), heights = 45)  %T>%
    # add style for all cells
    addStyle(sheet = "Sheet1", style = body_style,
             rows = 1:(nrow(deployment_log) + 1),
             cols = 1:ncol(deployment_log),
             gridExpand = TRUE, stack = TRUE) %T>%
    writeData(sheet = "Sheet1", x = deployment_log, headerStyle = header_style)

}
