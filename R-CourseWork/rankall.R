
rankall <- function(outcome, num="best") {
    
    #Error checking - invalid case

    if (!outcome %in% c("heart attack", "heart failure", "pneumonia")) {
        stop("invalid outcome")
    }
    
    #Finding the position for the given outcome string

    pos <- ifelse(outcome == "heart attack", 11, ifelse(outcome == "heart failure", 17, 23))
    

    #Read and coerce dataset 

    dataset <- read.csv("./rprog-data/outcome-of-care-measures.csv", colClasses="character")
    dataset[,pos] <- suppressWarnings(as.numeric(dataset[,pos]))
    dataset <- dataset[!is.na(dataset[,pos]),]

         #Sort our data by specified mortality rate and hospital name
    dataset.sorted <- dataset[order(dataset[,pos], dataset[,2], na.last=TRUE),]
    dataset.sorted <- dataset.sorted[!is.na(dataset.sorted[,pos]),]
    
    #Parse out and validate our num
    num <- ifelse(num == "best", 1, ifelse(num == "worst", length(dataset.sorted), as.numeric(num)))
    
    #Remove duplicate state names
    states <- sort(unique(dataset.sorted[,7]))
    
    #Function returns the hospital name for the given state at the specified rank.
    state_hospital_data <- function(state) {
        temp <- subset(dataset.sorted, State==state)
        temp <- temp[num, c(2,7,pos)]
        temp$State <- state
        return (temp)
    }


    state_data <- lapply(states, state_hospital_data)
    dframe <- as.data.frame(do.call(rbind, lapply(states, state_hospital_data)), row.names=states)

    final_frame <- dframe[,c(1,2)]
    final_frame <- na.omit(final_frame)
    colnames(final_frame ) <- c("hospital", "state")
    return(final_frame)
}