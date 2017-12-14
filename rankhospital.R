rankhospital <- function(state, outcome, num = "best") {
    # read data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    valid_outcomes <- c("heart attack","heart failure","pneumonia")
    
    # make sure that the state is valid
    if(!is.element(state, outcome_data$State)){
        stop("invalid state")
    }
    
    # make sure that the outcome is valid
    if(!is.element(outcome, valid_outcomes)){
        stop("invalid outcome")
    }
    
    # convert the data to numeric
    outcome_data[, 11] <- as.numeric(outcome_data[, 11])
    outcome_data[, 17] <- as.numeric(outcome_data[, 17])
    outcome_data[, 23] <- as.numeric(outcome_data[, 23])
    
    # set column based on outcome 
    if(outcome == 'heart attack'){
        # convert the appropriate column to a number
        col_number <- 11
    } else if(outcome == 'heart failure'){
        col_number <- 17
    } else if(outcome == 'pneumonia'){
        col_number <- 23
    }
    
    # subset out the state
    by_state <-subset(outcome_data, State==state)
    
    # remove the NAs
    by_state <- subset(by_state, !is.na(by_state[,col_number]))
     
    # sort the data
    by_state <- by_state[ order(by_state[,col_number], by_state[,2]),]
    
    ## Return hospital name in that state with the given rank
    if(num == 'best'){
        return (by_state[1,2])
    } else if(num == 'worst') {
        return (by_state[dim(by_state)[1],2])
    } else if(num <= dim(by_state)[1]){
        return(by_state[num,2])
    } else {
        return(NA)
    }
}
