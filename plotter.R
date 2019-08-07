# Case Study - An evaluation of market perspective
# Author: Maria Luiza
# August/2019

library(ggplot2)
library(scales)
library(RColorBrewer)

plot_month_value <- function(data, colx, coly, colz, year){
    # Reorder the data based on month name
    data[[colx]] <- factor(data[[colx]], levels = month.abb)
    
    # Month value in two bar graphs side by side
    p <- ggplot(data, aes(x=data[[colx]], y = value)) +
        geom_bar(aes(fill = variable), stat = "identity", position = "dodge")
    
    p <- p+labs(title = paste("Mean Value variation (", year, ")", sep=""),
                x="Month", y="Value")
    
    p <- p + scale_fill_brewer(palette="Set2")
    p
}

plot_executive_value <- function(data, exec, month, val, year){
    # Reorder the data based on month name 
    data[[month]] <- factor(data[[month]], levels = month.abb)
    
    # Absolute value (Deal-Lost) for each Sales Executive per month through years
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

plot_boxplot_summary <- function(data){
    p <- ggplot(data, aes(x=Year, y=Value, fill = Lost_Deal)) + 
        geom_boxplot()
    p <- p+labs(title = "Summary per Year")
    p <- p + scale_fill_brewer(palette="Set3")
    p <- p + geom_hline(yintercept = median(data$Value),
                        color = "red",
                        size=0.75,
                        linetype="dashed")
    print(p)
}

plot_pie_state <- function(data){
    p <- ggplot(data, aes(x="", y=Prop, fill=State)) +
        geom_bar(stat="identity", width=1)
    p <- p + coord_polar("y", start=0, direction=-1)
    p <- p + facet_grid(facets = .~Year+Lost_Deal)
    p <- p + labs(x=NULL, y=NULL, fill=NULL, title = "State Value by Years")
    p <- p + scale_fill_brewer(palette="BuGn")
    p <- p + geom_text(aes(label = paste0(round(Prop*100,2), "%")), position = position_stack(vjust = 0.5), size=3)
    p <- p + theme_gray() + theme(axis.line = element_blank(),
                                  axis.text = element_blank(),
                                  axis.ticks = element_blank())
    print(p)
}