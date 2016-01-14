rankall <- function(outcome, num = "best") {
    
    ## First step is to create a 'cheat sheet' for columns in main data frame
    ## Each named outcome corresponds to its data column
    outcomes <- c("heart attack" = 11, "heart failure" = 17, "pneumonia" = 23)
    
    ## Just checking validity of passed argument 'outcome'
    if ((outcome %in% names(outcomes)) == FALSE) {
        stop("invalid outcome")
    }
    
    ## Read in the data file and make all blanks/NA the same and don't allow the
    ## creation of factors for the strings, we want character vectors
    full_data <- read.csv("outcome-of-care-measures.csv", 
                          na.strings = "Not Available", stringsAsFactors = FALSE)
    
    ## Create a data frame with just the three needed columns: state, outcome, 
    ## and hospital.  Then give the columns easy names, convert the outcome
    ## column to numeric, and remove the non-complete records.
    my_data <- full_data[,c(2,7,outcomes[outcome])]
    names(my_data) <- c("hospital", "st", "outcome")
    my_data$outcome <- as.numeric(my_data$outcome)
    cmp_rec <- complete.cases(my_data)
    my_data <- subset(my_data,cmp_rec)
    
    ## Order my_data by state, then outcome, they hospital so in the next step
    ## if there is a tie, everything is already ranked correctly
    my_data <- my_data[with(my_data, order(st, outcome, hospital)),]
    
    ## Now we'll create a data frame of all the state subsets
    state_frame <- split(my_data, my_data$st)    

    ## now we'll take care of the num argument when best is supplied
    if(num == "best") {
        num <- 1
    } 
    
    ## now we'll pull out the ranked hospital for each state and stick it in a 
    ## vector, with special situation for "worst" since it's a different index
    ## for each state
    if(num == "worst"){
        result <- sapply(state_frame, function(blah) blah[nrow(blah),1])
    } else {
        result <- sapply(state_frame, function(blah) blah[num,1])
    } 
    
    ## Finally, put into data frame so output format looks as requested
    answer <- data.frame("hospital" = result, "state" = names(result), 
                         row.names = names(result))
    
}