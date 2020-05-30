
compile_HOBO_data <- function(path.HOBO, area.name, serial.table, deployment.range){
  
  # initialize dateframe for storing the output
  HOBO_dat <- data.frame(INDEX = as.character())
  
  # list files .xlsx files in the data folder
  dat.files <- list.files(path.HOBO, all.files = FALSE, pattern = "*.xlsx") 
  
  # loop over each HOBO file
  for(i in 1:length(dat.files)) {
    
    # import HOBO file i
    hobo.i_dat <- read_excel(paste(path.HOBO, dat.files[i], sep = "/"), col_names = FALSE)
    
    # extract serial number
    serial.i <- hobo.i_dat[1,1] %>% 
      separate(col = 1, into = c(NA, "SERIAL"), sep = ": ", remove = TRUE)
    serial.i <- paste("HOBO-", serial.i$SERIAL, sep = "")
    
    # use serial number to identify the variable and depth (from serial.table)
    variable_depth <- serial.table %>% 
      filter(SERIAL == serial.i)  %>% 
      select(var_depth)
    variable_depth <- variable_depth$var_depth
    
    # remove plot title row and select the first three columns
    hobo.i <- hobo.i_dat %>% 
      slice(-1) %>%             
      select(c(1:3))
    
    # extract date column header (includes GMT offset)
    date_ref <- hobo.i[1,2]$...2
    # extract temperature column header (includes units)
    temp_ref <- data.frame(hobo.i[1,3]$...3) %>% 
      rename("temp_ref" = 1) %>%
      separate(col = "temp_ref", into = c("temp_ref", NA), sep = 8)
    temp_ref <- temp_ref$temp_ref
    
    # format data 
    hobo.i <- hobo.i %>%
      slice(-1) %>%                                      # remove column headings
      select(INDEX = 1, DATE = 2, TEMPERATURE = 3) %>%   # rename columns (will be dropped for export)
      mutate(DATE = convert_to_datetime(DATE)) %>%       # convert DATE to datetime
      mutate(DATE = as.character(DATE)) %>%              # convert DATE to a character so can add in the column headings
      mutate(INDEX = as.character(round(as.numeric(INDEX), digits = 0)))  %>%  # make sure INDEX will have the same class and format for each sheet
      # add meta data rows (deployment date, serial number, variable-depth can be merged in Excel file)
      # Date and Time stay unmerged
      add_row(INDEX = as.character(-1), DATE= date_ref, TEMPERATURE = temp_ref, .before = 1) %>% 
      add_row(INDEX = as.character(-2), DATE= variable_depth, TEMPERATURE = variable_depth, .before = 1) %>%
      add_row(INDEX = as.character(-3), DATE= serial.i, TEMPERATURE = serial.i, .before = 1) %>% 
      add_row(INDEX = as.character(-4), DATE= deployment.range, TEMPERATURE = deployment.range, .before = 1) 
    
    # merge data on the INDEX row
    HOBO_dat <- full_join(HOBO_dat, hobo.i, by = "INDEX")
    
  }
  
  # extract the deployment date from deployment.range
  file.date <- separate(data = data.frame(deployment.range), 
                        col = deployment.range, 
                        into = c("file.date", NA, NA), sep  = " " ) %>% 
    as.character() %>% 
    as_date() %>% 
    format('%Y-%m-%d')
  
  # name of output file
  file.name <- paste(area.name, file.date, sep = " ")
  
  write_csv(HOBO_dat, path = paste(path, "/", file.name, ".csv", sep = ""), col_names = FALSE) 
  
}

