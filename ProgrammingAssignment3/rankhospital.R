rankhospital <- function(state, outcome, num = "best") {
    
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
    
    ## Pull a unique list of the states
    states <- as.vector(unique(full_data[,7]))
    
    ## Just checking validity of passed argument 'state'
    if ((state %in% states) == FALSE) {
        stop("invalid state")
    }
    
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
    
    ## Pull subset of needed state, which is already ordered.
    result <- subset(my_data, st == state, select = hospital)
    
    ## Now just pick which hospital you want out of the subset: best gets first,
    ## worst gets last, and anything else just gets that numbered index.
    if(num == "best") {
        return(result[1,])
    } else if(num == "worst") {
        return(result[nrow(result),])
    } else {
        return(result[num,])
    }
    
}