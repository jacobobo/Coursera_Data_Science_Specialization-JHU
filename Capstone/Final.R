library(tm)
library(SnowballC)

myCorpus <- Corpus(DirSource("Data/Sampled"), readerControl=list(reader=readPlain,language="en"))

myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, removeNumbers)

funkypunc <- function(x) {
    gsub("[^[:alnum:][:space:]']", "", x)
}
myCorpus <- tm_map(myCorpus, content_transformer(funkypunc)) ## function to remove funky characters
myCorpus <- tm_map(myCorpus, stripWhitespace) ## remove whitespace

library(RWeka)

## these commands create the tokenizing functions for each level of N-gram
unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1,delimiters=' \r\n\t'))
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2,delimiters=' \r\n\t'))
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3,delimiters=' \r\n\t'))
quadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4,delimiters=' \r\n\t'))

## these commands actually create the document term matrix for each N-gram
unigramDTM <- TermDocumentMatrix(myCorpus, control = list(tokenize=unigramTokenizer))
bigramDTM <- TermDocumentMatrix(myCorpus, control = list(tokenize=bigramTokenizer))
trigramDTM <- TermDocumentMatrix(myCorpus, control = list(tokenize=trigramTokenizer))
quadgramDTM <- TermDocumentMatrix(myCorpus, control = list(tokenize=quadgramTokenizer))

smallunigramDTM <- removeSparseTerms(unigramDTM, sparse = 0.34)
smallbigramDTM <- removeSparseTerms(bigramDTM, sparse = 0.34)
smalltrigramDTM <- removeSparseTerms(trigramDTM, sparse = 0.34)
smallquadgramDTM <- removeSparseTerms(quadgramDTM, sparse = 0.34)

library(slam)

frequni <- sort(row_sums(smallunigramDTM, na.rm=TRUE), decreasing=TRUE)
worduni <- names(frequni)
uniDF <- data.frame(word=worduni, freq=frequni)
rm(frequni,worduni)

freqbi <- sort(row_sums(smallbigramDTM, na.rm=TRUE), decreasing=TRUE)
wordbi <- strsplit(as.vector(names(freqbi)), split = " ")
wordbi <- do.call(rbind, wordbi)
biDF <- data.frame(word1=wordbi[,1], word2=wordbi[,2], freq=freqbi)
rm(freqbi,wordbi)

freqtri <- sort(row_sums(smalltrigramDTM, na.rm=TRUE), decreasing=TRUE)
wordtri <- strsplit(as.vector(names(freqtri)), split = " ")
wordtri <- do.call(rbind, wordtri)
triDF <- data.frame(word1=wordtri[,1], word2=wordtri[,2], word3=wordtri[,3], freq=freqtri)
rm(freqtri,wordtri)

freqquad <- sort(row_sums(smallquadgramDTM, na.rm=TRUE), decreasing=TRUE)
wordquad <- strsplit(as.vector(names(freqquad)), split = " ")
wordquad <- do.call(rbind, wordquad)
quadDF <- data.frame(word1=wordquad[,1], word2=wordquad[,2], word3=wordquad[,3], word4=wordquad[,4], freq=freqquad)
rm(freqquad,wordquad)

## cleaning up the input
## btest <- "1Like  him."
## ttest <- "  Your friends5 are,"
## qtest <- "Yourself iN the    mir$ror"

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
    return(head(as.character(unlist(x)),6))
}

LookInTri <- function(x) {
    x <- subset(triDF, word1 == x[1] & word2 == x[2], select = word3)
    return(head(as.character(unlist(x)),6))
}

LookInBi <- function(x) {
    x <- subset(biDF, word1 == x[1], select = word2)
    return(head(as.character(unlist(x)),6))
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
            if (length(results) == 0) break
        }
        x <- x[-1]
        len <- len-1
    }
    return(results)
}