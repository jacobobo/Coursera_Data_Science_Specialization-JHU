
## cleaning up the input
## btest <- "1Like  him."
## ttest <- "  Your friends5 are,"
## qtest <- "Yourself iN the    mir$ror"
## example1 <- c("yourself","in","the")
## example2 <- c("yourself","in","a")
## example3 <- c("yourself","in","zurich")
## example4 <- c("yourself","in","jumbalaya")

loadData <- function() {
    load("smallDFs.RData", .GlobalEnv)
}

CleanInput <- function(x) {
    x <- tolower(x)                              ## lowercase
    x <- gsub("[^[:alpha:][:space:]']", "", x)   ## no #'s or punc. except '
    x <- gsub("\\s+"," ",x)                      ## whitespace
    x <- gsub("^\\s+|\\s+$", "", x)              ## whitespace
    return(x)
}

CreateSearch <- function(userInput) {
    cleanWords <- CleanInput(userInput)
    cleanWords <- unlist(strsplit(cleanWords, " "))
    len <- length(cleanWords)
    if (len < 1) {
        stop("You haven't put enough words in, silly.")
    } else if (len == 1) {
        x <- as.character(cleanWords[1])
    } else if (len == 2) {
        x <- as.character(c(cleanWords[1],cleanWords[2]))
    } else {
        x <- as.character(c(cleanWords[len-2],cleanWords[len-1], cleanWords[len]))
    }
    return(x)
}

LookInQuad <- function(x) {
    x <- subset(quadDF, word1 == x[1] & word2 == x[2] & word3 == x[3], select = word4)
    return(head(as.character(unlist(x)),5))
}

LookInTri <- function(x) {
    x <- subset(triDF, word1 == x[1] & word2 == x[2], select = word3)
    return(head(as.character(unlist(x)),5))
}

LookInBi <- function(x) {
    x <- subset(biDF, word1 == x[1], select = word2)
    return(head(as.character(unlist(x)),5))
}

FindNextWord <- function(x) {
    len <- length(x)
    results <- c()
    while (length(results) == 0) {
        if (len ==3 ) {
            results <- LookInQuad(x)
        } else if (len == 2) {
            results <- LookInTri(x)
        } else {
            results <- LookInBi(x)
            if (length(results) == 0) {
                results <- as.character(uniDF[1:5,1])
            }
        }
        x <- x[-1]
        len <- len-1
    }
    return(as.data.frame(results))
}

