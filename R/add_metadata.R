#'@importFrom dplyr add_row

# function to add metadata rows to HOBO and aquaMeasure data
# called by compiled_hobo_data() and compile_aquaMeasure_data()

# data.char has three columns of class character: INDEX, DATE, PLACEHOLDER
# row1, row2, row3, and row4 are metadata to addin the DATE and PLACEHOLDER columns

add_metadata <- function(data.char, row1, row2, row3, row4){

  data.char %>%
    add_row(INDEX = as.character(-1), DATE = row4[1], PLACEHOLDER = row4[2], .before = 1) %>%
    add_row(INDEX = as.character(-2), DATE = row3, PLACEHOLDER = row3, .before = 1) %>%
    add_row(INDEX = as.character(-3), DATE = row2, PLACEHOLDER = row2, .before = 1) %>%
    add_row(INDEX = as.character(-4), DATE = row1, PLACEHOLDER = row1, .before = 1)
}


