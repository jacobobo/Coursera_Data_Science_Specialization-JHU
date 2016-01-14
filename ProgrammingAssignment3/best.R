## This function takes two arguments: the 2-character abbreviated name of a 
## state and an outcome name. The function reads the 
## outcome-of-care-measures.csv file and returns a character vector with the 
## name of the hospital that has the best (i.e. lowest) 30-day mortality for the
## specified outcome in that state. The hospital name is the name provided in 
## the Hospital.Name variable. The outcomes can be one of "heart attack", 
## "heart failure", or "pneumonia". Hospitals that do not have data on a 
## particular outcome should be excluded from the set of hospitals when deciding
## the rankings. 
 
## Handling ties. If there is a tie for the best hospital for a given outcome,
## then the hospital names should be sorted in alphabetical order and the first
## hospital in that set should be chosen (i.e. if hospitals "b", "c", and "f" 
## are tied for best, then hospital "b" should be returned).
 
## The function also checks the validity of its arguments. If an invalid state 
## value is passed to best, the function should throw an error via the stop 
## function with the exact message "invalid state". If an invalid outcome value
## is passed to best, the function should throw an error via the stop function
## with the exact message "invalid outcome".

best <- function(state, outcome) {
    
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
    
    ## Pull subset of needed state, which is already ordered, and just pop the
    ## first one. Success!
    result <- subset(my_data, st == state, select = hospital)
    result[1,]
}

## Here is some sample output from the function.
## > source("best.R")
## > best("TX", "heart attack")
## [1] "CYPRESS FAIRBANKS MEDICAL CENTER"
## > best("TX", "heart failure")
## [1] "FORT DUNCAN MEDICAL CENTER"
## > best("MD", "heart attack")
## [1] "JOHNS HOPKINS HOSPITAL, THE"
## > best("MD", "pneumonia")
## [1] "GREATER BALTIMORE MEDICAL CENTER"
## > best("BB", "heart attack")
## Error in best("BB", "heart attack") : invalid state
## > best("NY", "hert attack")
## Error in best("NY", "hert attack") : invalid outcome