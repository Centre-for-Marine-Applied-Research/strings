
## loop over variables instead of doing loop twice


compile_aquaMeasure_data <- function(path.aM, area.name, vars.aM = c("Temperature", "Dissolved Oxygen", "Salinity"), 
                                     depth.aM, deployment.range){
  
  # # Error message in case trying to plot a variable that is not in the dataframe OR
  # # a variable is spelled wrong 
  # if(any(!(vars.to.plot %in%  unique(dat.tidy$VARIABLE)))){
  #   
  #   stop("You are trying to plot a variable that is not in this dataframe. Check spelling in vars.to.plot")
  # }
  
  # initialize dateframe for storing the output
  aM_dat <- data.frame(INDEX = as.character())
  
  # list files in the data folder
  dat.files <- list.files(path.aM, all.files = FALSE) 
  aM_dat_raw <- read_csv(paste(path, dat.files[1], sep = "/"), col_names = TRUE)
  
  serial <- aM_dat_raw$Sensor[1]
  
  # extract date column header (includes UTC offset)
  date_ref <- names(aM_dat_raw)[2]
 
  for(i in 1:length(vars.aM)){
  
  aM.i <- aM_dat_raw %>% 
    select(`Timestamp(UTC)`, `Record Type`, vars.aM[i]) %>% 
    filter(`Record Type` == vars.aM[i]) %>% 
    rename(PLACEHOLDER = 3) %>%
    mutate(INDEX = as.character(c(1:n())))
  
  if(vars.aM[i] == "Dissolved Oxygen") aM.i <- aM.i %>% filter(PLACEHOLDER > 0)
  
  aM.i <- aM.i %>% 
    transmute(INDEX, DATE = as.character(`Timestamp(UTC)`), PLACEHOLDER = as.character(PLACEHOLDER)) %>% 
    # add meta data rows (deployment date, serial number, variable-depth can be merged in Excel file)
    # Date and Time stay unmerged
    add_row(INDEX = as.character(-1), 
            DATE= date_ref, PLACEHOLDER = vars.aM[i], .before = 1) %>% 
    add_row(INDEX = as.character(-2), 
            DATE= paste(vars.aM[i], depth, sep = "-"), PLACEHOLDER = paste(vars.aM[i], depth, sep = "-"), .before = 1) %>%
    add_row(INDEX = as.character(-3), 
            DATE= serial, PLACEHOLDER = serial, .before = 1) %>% 
    add_row(INDEX = as.character(-4), 
            DATE= deployment.range, PLACEHOLDER = deployment.range, .before = 1) 
  
  # merge data on the INDEX row
  aM_dat <- full_join(aM_dat, aM.i, by = "INDEX")
  
  }
  
  # extract the deployment date from deployment.range
  file.date <- separate(data = data.frame(deployment.range), 
                        col = deployment.range, 
                        into = c("file.date", NA, NA), sep  = " " ) %>% 
    as.character() %>% 
    as_date() %>% 
    format('%Y-%m-%d')
  
  # vars
  TEMP <- ifelse(any(vars.aM %in% "Temperature"), temp <- "_TEMP", "")
  DO <- ifelse(any(vars.aM %in% "Dissolved Oxygen"), "_DO", "")
  SAL <- ifelse(any(vars.aM %in% "Salinity"), "_SAL", "")
  
  # name of output file
  file.name <- paste(area.name, "_", file.date, TEMP, DO, SAL, sep = "")
  
  write_csv(aM_dat, path = paste(path, "/", file.name, ".csv", sep = ""), col_names = FALSE) 
  
}