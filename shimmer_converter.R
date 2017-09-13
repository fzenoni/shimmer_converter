shimmer_converter <- function(input_folder, output_folder) {
  
  library(data.table)
  library(dplyr)
  library(lubridate)
  
  ## Some options to keep digits and ignore scientific notation
  options(scipen = 999, digits = 15)
  
  ## List files in input folder
  files_list <- list.files(input_folder, full.names = TRUE)
  
  ## Remove logfile if it exists
  if(file.exists('logfile.txt')) {
    file.remove('logfile.txt')
  }
  
  ## Open connection to logfile.txt
  file_conn <- file('logfile.txt', 'a')
  
  ## Beginning of workhorse function
  read_and_convert <- function(file) {
    
    ## Column names change according to Shimmer sensor serial number
    if(grepl('CD4B', basename(file))) {
      options(warn = -1)
      data <- fread(file, select = c('idCD4B_Timestamp_Unix_CAL',
        'idCD4B_GSR_Skin_Conductance_CAL',
        'idCD4B_PPGtoHR_LPF_CAL'))
      options(warn = 0)
    } else if(grepl('CD33', basename(file))) {
      options(warn = -1)
      data <- fread(file, select = c('Shimmer_CD33_Timestamp_Unix_CAL',
        'Shimmer_CD33_GSR_Skin_Conductance_CAL',
        'Shimmer_CD33_PPGtoHR_CAL'))
      options(warn = 0)
    } else {
      log <- paste('File', basename(file), 'was ignored.', sep = ' ')
      # writeLines(log, file_conn)
      cat(log,'\n')
      return()
    }
    
    ## Rename columns
    colnames(data) <- c('time', 'conductance', 'BPM')
    ## Remove first row containig unit names
    data <- data[-1]
    
    ## Convert from string to numeric
    ## And convert from ms to s
    data[, conductance := as.numeric(conductance)]
    data[, time := as.numeric(time)/1000.]
    data[, time := time - time[1]]
    data[, BPM := as.numeric(BPM)]

    ## Addition of third column for events setting
    ## We have none but we must provide it anyway
    data$off <- 0
    
    ## Save table as text file
    write.table(data[, list(time, conductance, off)], paste0(output_folder, '/',
      basename(file), '_ledalab.txt'), sep = ' ', row.names = FALSE,
      col.names = FALSE)
    
    write.table(data[, list(time, BPM)], paste0(output_folder, '/',
      basename(file), '_BPM.txt'), sep = ' ', row.names = FALSE,
      col.names = FALSE)
    
    timestamp <- as.POSIXct(data[1,time], origin = '1970-01-01')
    log <- paste(basename(file), timestamp, sep = ': ')
    writeLines(log, file_conn)
    cat(log, '\n')
    
  }
  
  ## Loop over all files in folder
  ## TODO: do it in the purrr way
  x <- sapply(files_list, read_and_convert)
  close(file_conn)
  
}
