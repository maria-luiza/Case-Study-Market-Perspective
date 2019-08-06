# Case Study - An evaluation of market perspective
# Author: Maria Luiza N. Rodrigues
# August/2019

source("utils.R") # Functions to load and clean data
source("analysis.R") # Functions to extract info on data
source("plotter.R") # Functions to plotter data

library(reshape2)
require(gridExtra)

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
    

    data_gen[[i]] <- list(sum_years, max_year_general, min_year_general, data_per_year)
    names(data_gen[[i]]) <- c("Summary", "MaxVal", "MinVal", "Data")
    
    # Remove unused data
    rm(data_deals)
    rm(sum_years, max_year_general, min_year_general)
}

names(data_gen) <- c("Deal", "Lost")

# 4. Data Evaluation

# # 4.1 Value per month per Year
# 
# plots <- list()
# for(year in years){
#     d1 <- aggregate(data_gen$Deal$Data[[year]]$Value ~ data_gen$Deal$Data[[year]]$Month,
#                     data_gen$Deal$Data[[year]],
#                     mean)
#     d2 <- aggregate(data_gen$Lost$Data[[year]]$Value ~ data_gen$Lost$Data[[year]]$Month,
#                     data_gen$Lost$Data[[year]],
#                     mean)
# 
#     names(d2) <- c("Month", "Lost Value")
#     d1 <- cbind(d1, d2$`Lost Value`)
# 
#     names(d1) <- c("Month", "Deal Value", "Lost Value")
#     d1$Month <- with(d1, month.abb[Month])
#     d1 <- d1[order(match(d1$Month, month.name)), ]
# 
#     dfm <- melt(d1[,c('Month','Deal Value','Lost Value')],id.vars = 1)
#     plots[[year]] <- plot_month_value(dfm, 1, 2, 3, year)
# }
# do.call(grid.arrange, c(plots, nrow=length(years)))

# 4.2 Executive sales thourgh years

plots <- list()
for(year in years){
    d1 <- aggregate(data_gen$Deal$Data[[year]]$Value ~ data_gen$Deal$Data[[year]]$Sales_Executive + data_gen$Deal$Data[[year]]$Month,
                data_gen$Deal$Data[[year]], mean)
    d2 <- aggregate(data_gen$Lost$Data[[year]]$Value ~ data_gen$Lost$Data[[year]]$Sales_Executive + data_gen$Lost$Data[[year]]$Month,
                data_gen$Lost$Data[[year]], mean)

    names(d2) <- c("Executive", "Month", "Lost Value")
    d1 <- cbind(d1, d2$`Lost Value`)

    names(d1) <- c("Executive", "Month", "Deal Value", "Lost Value")

    d1$Value <- with(d1, `Deal Value` - `Lost Value`)

    d1$Month <- month.abb[match(d1$Month, month.name)]
    plots[[year]] <- plot_executive_value(d1, "Executive", "Month", "Value", year)
}

do.call(grid.arrange, c(plots, nrow=length(years)))

# # 4.3 Get information about client who's paid more
# clients <- list()
# for(year in years){
#     data <- data_gen$Deal
#     mean_year <- mean(data$Data[[year]]$Value)
#     sd_year <- sd(data$Data[[year]]$Value)
#     
#     client <- names(which.max(table(data$Data[[year]]$Client)))
#     print(data$Data[[year]][data$Data[[year]]$Client == client, ])
#     
#     
#     # print(data$MaxVal[[year]])
#     # print(paste0("Mean: ", mean_year, ", Sd: ", sd_year))
# }
# # 5. Plotter data
