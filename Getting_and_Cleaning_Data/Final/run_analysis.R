## Final Assignment for Getting and Cleaning Data
## The cleaning and and creation of a tidy data set of data collected from the 
## accelerometers of a Samsung Galaxy S Smartphone

run_analysis <- function(){

    ## First step is to read in all of the data into temporary tables.
    ## I'm using strip.write to make sure nothing funky sneaks in, and
    ## also check.names when I'm naming columns so they don't get deprecated.
    
    activities <- read.table("activity_labels.txt", strip.white = TRUE)
    features <- read.table("features.txt", strip.white = TRUE)
    xtest <- read.table("./test/X_test.txt", col.names = features$V2, 
                            check.names = FALSE, strip.white = TRUE)
    subtest <- read.table("./test/subject_test.txt", strip.white = TRUE)
    activitytest <- read.table("./test/y_test.txt", strip.white = TRUE)
    xtrain <- read.table("./train/X_train.txt", col.names = features$V2, 
                            check.names = FALSE, strip.white = TRUE)
    subtrain <- read.table("./train/subject_train.txt", strip.white = TRUE)
    activitytrain <- read.table("./train/y_train.txt", strip.white = TRUE)
    
    ## Now that we've got all the data, I'm going to column bind both sets
    ## together, and then rowbind both sets into one uber-dataframe
    
    x <- as.data.frame(cbind(subtest,activitytest,xtest))
    y <- as.data.frame(cbind(subtrain, activitytrain, xtrain))
    uberdf <- rbind(x,y)
    
    ## To conserve memory, I'll remove all the temporary files I no longer need
    
    rm(features,subtest,activitytest,xtest,subtrain, activitytrain, xtrain, 
                            x, y)
    
    ## Now we're going to extract all the mean and standard deviation columns
    ## by first doing a logical text search for "mean()" and "std()" and then
    ## using that to subset the uber dataframe into a new one.
    
    goodcolumns <- grepl("mean\\(\\)|std\\(\\)", names(uberdf))
    goodframe <- data.frame(uberdf[,1:2], uberdf[,goodcolumns], 
                            check.names = FALSE)
    
    ## Remove the uber frame since it's no longer needed and large
    
    rm(uberdf)
    
    ## We'll go through the activity column and replace the indexes with the
    ## actual names of the activity
    
    for (i in 1:nrow(activities)) { 
        goodframe$V1.1 <- sub(activities[i,1],activities[i,2],goodframe$V1.1)
    }
    
    ## Remove more unneeded items
    
    rm(activities, goodcolumns, i)
    
    ## Rename all the variables to something nice and easy
    
    names(goodframe)[1:2] <- c("subject", "activity")
    names(goodframe) <- tolower(gsub("\\-","_",names(goodframe)))
    names(goodframe) <- sub("\\(\\)","",names(goodframe))
    
    ## Sort the data by Subject and Activity
    
    goodframe <- arrange(goodframe,subject,activity)
    
    ## Create the nice tidy data set that reduces to one mean value for each 
    ## variable associated with each activity and each subject
    
    tidy <- ddply(goodframe,.(subject, activity),numcolwise(mean))
}
