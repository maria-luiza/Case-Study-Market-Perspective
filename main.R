# Case Study - An evaluation of market perspective
# Author: Maria Luiza N. Rodrigues
# August/2019

source("utils.R") # Functions to load and clean data
source("analysis.R") # Functions to extract info on data
source("plotter.R") # Functions to plotter data

library(reshape2)
require(plyr)
require(dplyr)
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

# 4.1 Value per month per Year
png(filename = "value_month_year.png", width = 1200, height = 600, units = "px", res=100)

plots <- list()
for(year in years){
    d1 <- aggregate(data_gen$Deal$Data[[year]]$Value ~ data_gen$Deal$Data[[year]]$Month,
                    data_gen$Deal$Data[[year]],
                    mean)
    d2 <- aggregate(data_gen$Lost$Data[[year]]$Value ~ data_gen$Lost$Data[[year]]$Month,
                    data_gen$Lost$Data[[year]],
                    mean)

    names(d2) <- c("Month", "Lost Value")
    d1 <- cbind(d1, d2$`Lost Value`)

    names(d1) <- c("Month", "Deal Value", "Lost Value")
    d1$Month <- with(d1, month.abb[Month])
    d1 <- d1[order(match(d1$Month, month.name)), ]

    dfm <- melt(d1[,c('Month','Deal Value','Lost Value')],id.vars = 1)
    plots[[year]] <- plot_month_value(dfm, 1, 2, 3, year)
}
do.call(grid.arrange, c(plots, nrow=length(years)))
dev.off()

# 4.2 Sales executive thourgh years
png(filename = "sales_executive_years.png", width = 770, height = 665, units = "px", res=100)

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
dev.off()

# 4.3 Get information about client who's paid more
clients <- list()
deals <- c("Deal", "Lost")
for(year in years){
    for(deal in deals){
        data1 <- data_gen[[deal]]
        # Get client most frequent through the year
        client <- names(which.max(table(data1$Data[[year]]$Client)))

        # Get information about him
        client_info <- data1$Data[[year]][data1$Data[[year]]$Client == client, ]
        mean_client <- mean(client_info$Value)
        executives <- unique(client_info$Sales_Executive)

        clients[[deal]][[year]] <- list(client, mean_client, executives)
    }
}

print(clients)

# 4.4 Summary Boxplot
png(filename = "summary_years.png", width = 825, height = 660, units = "px", res=150)

# Get only information used to evaluate
data_info <- select(data, Year, State, Value, Lost_Deal)
plot_boxplot_summary(data_info)
dev.off()

# # 4.5 State Report
png(filename = "state_report.png", width = 1600, height = 800, units = "px", res=150)
data_info <- aggregate(. ~State + Year + Lost_Deal, data=data_info, sum, na.rm=TRUE)
prop_data <-ddply(data_info,.(Year, Lost_Deal),transform,Prop=(Value/sum(Value)))

prop_data <- prop_data %>% mutate(lab.ypos = cumsum(Prop) - 0.5*Prop)

# Rename type of lost_deal (True -> Lost, False -> Deal)
levels(prop_data$Lost_Deal) <- c("Deal", "Lost_Deal", "Lost")

plot_pie_state(prop_data)
dev.off()
