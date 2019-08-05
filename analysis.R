# Case Study - An evaluation of market perspective
# Author: Maria Luiza N. Rodrigues
# August/2019

# The functions below will help us to extract
# some information about the data

group_by_year <- function(data, year){
    out <- NULL
    # Filter the data based on Year
    if(year %in% data$Year){
        out <- subset(data, Year == year)
    }
    else{
        stop("Information not available for this year.")
    }
    out
}

group_by_state <- function(data, state){
    # Filter the data based on State
    out <- NULL
    if(state %in% data$State){
        out <- subset(data, State == state)
    }
    else{
        stop("Information not available for this State.")
    }
    out
}

max_Value <- function(data){
    #Get information about negotiation with max value
    out <- data[which.max(data$Value),]
    out
}

min_Value <- function(data){
    #Get information about negotiation with min value
    out <- data[which.min(data$Value),]
    out
}

lost_value <- function(data, bool){
    # Subset the dataset based on bool
    out <- subset(data, Lost_Deal ==  bool)
    out
}

general_info <- function(data){
    # Overall
    summ <- summary(data$Value)
    
    # Peak
    max_state_general <- max_Value(data)
    
    # Valley
    min_state_general <- min_Value(data)
    
    # Summarize the general information
    out <- list(summ, max_state_general, min_state_general)
    names(out) <- c("Summary", "MaxValue", "MinValue")
    
    out
}