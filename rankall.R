rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that the outcome is valid
    valid_outcomes <- c("heart attack","heart failure","pneumonia")
    if(!is.element(outcome, valid_outcomes)){
        stop("invalid outcome")
    }
    
    # create the empty result array
    states <- sort(unique(outcome_data$State))
    hospital_list <- rep(NA, length(states))
    
    # set column based on outcome 
    if(outcome == 'heart attack'){
        # convert the appropriate column to a number
        col_number <- 11
    } else if(outcome == 'heart failure'){
        col_number <- 17
    } else if(outcome == 'pneumonia'){
        col_number <- 23
    }
    
    # convert the data to numeric
    outcome_data[, 11] <- as.numeric(outcome_data[, 11])
    outcome_data[, 17] <- as.numeric(outcome_data[, 17])
    outcome_data[, 23] <- as.numeric(outcome_data[, 23])
    
    # loop through each state and find the hospital of the given rank
    for(i in 1:length(states)){
        current_state <- states[i]
        by_state <-subset(outcome_data, State==current_state)
        
        # sort the data
        by_state <- by_state[ order(by_state[,col_number], by_state[,2]),]
        
        # remove the NAs
        by_state <- subset(by_state, !is.na(by_state[,col_number]))
        
        if(num == 'best'){
            hospital <- by_state[1,2]
        } else if (num == 'worst') {
            hospital <- by_state[dim(by_state)[1],2]
        } else if(num <= dim(by_state)[1]){
            hospital <- by_state[num,2]
        } else {
            hospital <- NA
        }
       
        # add the result to the list
        hospital_list[i] <- hospital
    }
    
    # combine the list of states with the hospitals for our data frame
    return_data <- data.frame(hospital=hospital_list, state=states)
    return_data
}
