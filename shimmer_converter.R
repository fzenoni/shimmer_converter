shimmer_converter <- function(input_folder, output_folder) {
  
  library(data.table)
  library(dplyr)
  
  ## Some options to keep digits and ignore scientific notation
  options(scipen = 999, digits = 15)
  
  ## List files in input folder
  files_list <- list.files(input_folder, full.names = TRUE)
  
  ## Open connection to logfile.txt
  file_conn <- file('logfile.txt', 'a')
  
  ## Beginning of workhorse function
  read_and_convert <- function(file) {
    
    ## Column names change according to Shimmer sensor serial number
    if(grepl('CD4B', basename(file))) {
      options(warn = -1)
      data <- fread(file, select = c('idCD4B_Timestamp_Unix_CAL', 'idCD4B_GSR_Skin_Conductance_CAL'))
      options(warn = 0)
    } else if(grepl('CD33', basename(file))) {
      options(warn = -1)
      data <- fread(file, select = c('Shimmer_CD33_Timestamp_Unix_CAL', 'Shimmer_CD33_GSR_Skin_Conductance_CAL'))
      options(warn = 0)
    } else {
      log <- paste('File', basename(file), 'was ignored.', sep = ' ')
      writeLines(log, file_conn)
      cat(log,'\n')
      return()
    }
    
    ## Rename columns
    colnames(data) <- c('time', 'conductance')
    ## Remove first row containig unit names
    data <- data[-1, ]
    ## Reset row numbers
    rownames(data) <- NULL
    
    ## Convert from string to numeric
    data$conductance <- as.numeric(data$conductance)
    data$time <- as.numeric(data$time)
    
    ## Convert time from ms to s
    data$time <- data$time/1000.  
    
    ## Addition of third column for events setting
    ## We have none but we must provide it anyway
    data[, off := 0]
    
    ## Save table as text file
    write.table(data, paste0(output_folder, basename(file), '_ledalab.txt'), sep = ' ',
      row.names = FALSE, col.names = FALSE)
    
    log <- paste('File', basename(file), 'OK!', sep = ' ')
    writeLines(log, file_conn)
    cat(log, '\n')
    
  }
  
  ## Loop over all files in folder
  ## TODO: do it in the purrr way
  x <- sapply(files_list, read_and_convert)
  close(file_conn)
  
}