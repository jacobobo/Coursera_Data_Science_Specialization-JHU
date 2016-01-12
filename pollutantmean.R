pollutantmean <- function(directory, pollutant, id = 1:332) {
    
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
    
    ## then get the mean of the specific column, disregarding NA's
    pollutant_avg <- mean(output[,pollutant], na.rm = TRUE)
    
    pollutant_avg
}