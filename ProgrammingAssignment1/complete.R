complete <- function(directory, id = 1:332) {
    ## first, we'll create a list of all the file names in the provided
    ## directory.  use full.names = TRUE to get the local path of file.
    files_full <- list.files(directory, full.names = TRUE)
    
    ## then we'll pull out only the files we want based on ID
    used_files <- files_full[id]
    
    ## now we'll create a temporary data frame by reading each of the .csv files
    ## into the frame - inception of frames, data frame full of data frames
    tmp <- lapply(used_files, read.csv)
    
    ## finally, we'll bind the all the rows together so it's one giant frame
    output <- do.call(rbind, tmp)
    
    ## returns a logical vector of which rows contain complete records
    full_cases <- is.na(output[,2]) == FALSE & is.na(output[,3]) == FALSE
    
    ## subsetting output for only the complete records
    full_data <- output[full_cases,]
    
    ## create a data frame by merging 1st row of id, with 1st row of a table
    ## that contains the frequency count of ID in the full_data table
    ## done this way for the sole purpose of keeping the order provided by 
    ## id argument at beginning of file. (i.e. if shown as downcount, 30:25)
    count_table <- merge(id, table(full_data$ID), by.x=1, by.y=1, sort=FALSE)
    
    ## add requested column names
    colnames(count_table) <- c("id", "nobs")
    
    count_table
}