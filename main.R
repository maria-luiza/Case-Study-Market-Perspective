# Case Study - An evaluation of market perspective
# Author: Maria Luiza N. Rodrigues
# August/2019

source("utils.R") # Functions to load and clean data
source("analysis.R") # Functions to extract info on data
source("plotter.R") # Functions to plotter data

# 1. Load the data
data_file <- load_data("Data/", "Test_data.csv")
# 2. Clean the data
data <- clean_data(data_file)

# Remove unsed data
rm(data_file)

# 3. Extract general information

# List of States and years
states <- unique(data$State)
years <- unique(sapply(data$Year, as.character))

# Lost deals
deals <- sort(unique(data$Lost_Deal))

data_gen <- list()
for(i in seq_along(deals)){
    data_deals <- lost_value(data, deals[i])
    
    # State summary
    sum_states <- list()
    
    #Max values per State
    max_state_general <- list()

    #Min values per State
    min_state_general <- list()
    
    data_per_state <- list()
    for(state in states){
        # Considering that we have information over the years
        # We should get information in general and through the years
        data_per_state[[state]] <- group_by_state(data_deals, state)
        
        # Get general information from data_per_state
        state_info <- general_info(data_per_state[[state]])
        
        # Overall
        sum_states[[state]] <- state_info$Summary

        # Peak
        max_state_general[[state]] <- state_info$MaxValue

        # Valley
        min_state_general[[state]] <- state_info$MinValue
    }
    
    # Years summary
    sum_years <- list()

    #Max values per Year
    max_year_general <- list()

    #Min values per Year
    min_year_general <- list()
    
    data_per_year <- list()
    for(year in years){
        # Get information per year
        data_per_year[[year]] <- group_by_year(data_deals, year)
        
        year_info <- general_info(data_per_year[[year]])
        
        # Overall
        sum_years[[year]] <- year_info$Summary
        
        # Peak
        max_year_general[[year]] <- year_info$MaxValue
        
        # Valley
        min_year_general[[year]] <- year_info$MinValue
    }
    
    # General State information
    sum_info <- list(sum_states, sum_years)
    max_val <- list(max_state_general, max_year_general)
    min_val <- list(min_state_general, min_year_general)
    
    # Rename the variables
    names(sum_info) <- c("State", "Years")
    names(max_val) <- c("State", "Years")
    names(min_val) <- c("State", "Years")
    
    data_gen[[i]] <- list(sum_info, max_val, min_val)
    names(data_gen[[i]]) <- c("Summary", "MaxVal", "MinVal")
    
    # Remove unused data
    rm(data_deals)
    rm(sum_states, sum_years)
    rm(max_state_general, max_year_general)
    rm(min_state_general, min_year_general)
    rm(state_info, year_info)
    rm(sum_info, max_val, min_val)
}

names(data_gen) <- c("Deal", "Lost")

# 4. Data Evaluation

# 4.1 Value per month per Year

# par(mfrow=c(length(years), 1))
# for(year in years){
#     d <- aggregate(data_per_year[[year]]$Value ~ data_per_year[[year]]$Month,
#               data_per_year[[year]],
#               mean)
#     names(d) <- c("Month", "Mean of Value")
#     d <- d[order(match(d$Month, month.name)), ]
#     plot_month_value(d, 1, 2, year)
# }

# 4.2 Number of executives
par(mfrow=c(length(years), 1))
for(year in years){
    d <- aggregate(data_per_year[[year]]$Value ~ data_per_year[[year]]$Sales_Executive + data_per_year[[year]]$Month,
                   data_per_year[[year]],
                   mean)
    names(d) <- c("Executive", "Month", "Mean of Value")
    d <- d[order(match(d$Month, month.name)), ]
    plot_executive_value(d, 1, 2, 3)
}


# 5. Plotter data
