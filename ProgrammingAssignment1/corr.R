corr <- function(directory, threshold =0) {
    
    ##
    complete_records <- complete(directory)
    over_threshold_files <- subset(complete_records, complete_records$nobs > threshold)
    
    if (length(over_threshold_files$id) > 0){
        ## first, we'll create a list of all the file names in the provided
        ## directory.  use full.names = TRUE to get the local path of file.
        files_full <- list.files(directory, full.names = TRUE)
        
        ## then we'll pull out only the files we want based on ID
        used_files <- files_full[over_threshold_files$id]
        
        ## now we'll find the number of files, so we can make a correctly sized 
        ## empty vector to hold our correlation coefficients
        
        answer_vector <- vector(mode = "numeric", length = length(used_files))
        
        ## now we'll create a temporary data frame by reading each of the .csv files
        ## into the frame - inception of frames, data frame full of data frames
        ##tmp <- lapply(used_files, read.csv)
        
        for(i in 1:length(used_files)) {
            tmp <- read.csv(used_files[i])
            full_cases <- is.na(tmp[,2]) == FALSE & is.na(tmp[,3]) == FALSE
            full_data <- tmp[full_cases,]
            answer_vector[i] <- cor(full_data[,2],full_data[,3])
        }
        
        return(answer_vector)
    } else {
        return(c())
    }
    
    
    ## finally, we'll bind the all the rows together so it's one giant frame
    ##output <- do.call(rbind, tmp)
    
    ## returns a logical vector of which rows contain complete records
    ##full_cases <- is.na(output[,2]) == FALSE & is.na(output[,3]) == FALSE
    
    ## subsetting output for only the complete records
    ##full_data <- output[full_cases,]
}