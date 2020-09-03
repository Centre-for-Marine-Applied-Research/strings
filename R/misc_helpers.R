#'@title Extracts the extension of a file name
#'@details Extracts the file exension from a character string using //. as the
#'  separator.
#'@param file.name Character string of a file name. Must only include one ".",
#'  which is used as the seprator.
#'@export

#' @importFrom tidyr separate

extract_file_extension <- function(file.name){

  extension <- file.name %>%
    data.frame() %>%
    separate(col = 1, into = c(NA, "EXT"), sep = "\\.")

  extension$EXT
}





#'@title Binds together all csv files in a folder
#'@details All files must have the same column names.
#'@param path Path to the folder with the csv files to bind.
#'@return Returns a dataframe
#'@export

#' @importFrom readr read_csv

rbind_csv_files <- function(path){

  filenames_list <- list.files(path= path, full.names=TRUE, pattern = "*csv")

  All <- lapply(filenames_list, function(filename){
    print(paste("Merging", filename, sep = " "))
    read_csv(filename)
  })

}
