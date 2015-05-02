rankhospital <- function(state, outcome, num="best") {

    #Error checking - invalid case

    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    
    #Finding the position for the given outcome string

    pos <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    

    #Read and coerce dataset 

    dataset <- read.csv("./rprog-data/outcome-of-care-measures.csv", colClasses="character")
    dataset[,pos] <- suppressWarnings(as.numeric(dataset[,pos]))
    dataset <- na.omit(dataset)
    
    states <- table(dataset$State)
    if (!state %in% names(states)) { 
        stop("invalid state")
    }
    
    #extract data by the given state and sorting it

    temp <- subset(dataset, State==state)
    temp <- temp[order(temp[,pos], temp[,2], na.last=TRUE),2]
    temp <- na.omit(temp)
    
    num <- ifelse(num == "best", 1, ifelse(num == "worst", length(temp), as.numeric(num)))
    
    #Get hospital name for the given rank by its 30-day mortality rate
    temp[num]
}