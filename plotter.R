# Case Study - An evaluation of market perspective
# Author: Maria Luiza
# August/2019

library(ggplot2)
library(RColorBrewer)

plot_month_value <- function(data, colx, coly, colz, year){
    data[[colx]] <- factor(data[[colx]], levels = month.name)
    
    p <- ggplot(data, aes(x=data[[colx]], y = value)) +
        geom_bar(aes(fill = variable), stat = "identity", position = "dodge")
    
    p <- p+labs(title = paste("Mean Value variation (", year, ")", sep=""),
                x="Month", y="Value")
    
    p <- p + scale_fill_brewer(palette="Set2")
    p
}

plot_executive_value <- function(data, exec, month, val, year){
    data[[month]] <- factor(data[[month]], levels = month.abb)

    p <- ggplot(data,
                aes(x = data[[month]], y = data[[val]], group = data[[exec]], col = factor(data[[exec]])),
                color=factor(data[[exec]]))
    
    p <- p+labs(title = paste0(val, " per Executive (", year, ")"),
                x="Month", y="Absolute Value", color=exec)

    p <- p + geom_line() + geom_point()
    
    p <- p + geom_hline(yintercept = 0,
                        color = "black",
                        size=0.75,
                        linetype="dashed")
    p
}