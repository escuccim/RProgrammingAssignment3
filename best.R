best <- function(state,outcome){
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
    
    # get the proper column number for the outcome
    if(outcome == 'heart attack'){
        # convert the appropriate column to a number
        col_number = 11
    } else if(outcome == 'heart failure'){
        col_number = 17
    } else if(outcome == 'pneumonia'){
        col_number = 23
    }
    
    # subset the data by state
    by_state <-subset(outcome_data, State==state)
    # get the data for the selected outcome
    outcome_by_state <- by_state[, col_number]
    # find the minimum
    best_outcomes <- min(outcome_by_state, na.rm=TRUE)
    best_index <- which(outcome_by_state == best_outcomes)
    hospital_name <- by_state[best_index, 2]
    
    # if more than one result is returned, sort the list and return the first one
    if(length(hospital_name) > 1){
        best_hospital_list <- sort(hospital_name)
        return(best_hospital_list[1])    
    } else {
        # else return the hospital name
        return(hospital_name)
    }
    
    
    
}