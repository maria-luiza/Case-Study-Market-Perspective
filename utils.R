# Case Study - An evaluation of market perspective
# Author: Maria Luiza N. Rodrigues
# August/2019

load_data <- function(path, file){
    # Read .csv available in Data/
    data <- read.csv(
        paste(path, file, sep=""),
        header=FALSE,
        sep=',',
        na.strings = "",
        comment.char = "#"
    )
    # Get the name of columns
    cnames <- unlist(data[1,], use.names=FALSE)
    # Assign columns names to table
    names(data) <- make.names(cnames)
    data <- data[-1,]
    data # Return the table loaded
}

clean_data <- function(data){
    # Suposing the data was loaded
    cols <- names(data)
    #1. Remove empty rows
    data_full <- data[complete.cases(data), ]
    # 2. Convert columns with values into numeric
    data_full$Value <- suppressWarnings(as.numeric(data_full$Value))
    # 3. Apply uppercase in State column
    data_full$State <- toupper(data_full$State)
    # 4. Order data by year
    out <- data_full[order(data_full$Year), ]
    
    out
}