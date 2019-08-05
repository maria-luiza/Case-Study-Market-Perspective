# Case Study - An evaluation of market perspective
# Author: Maria Luiza
# August/2019

library(ggplot2)
library(RColorBrewer)

plot_month_value <- function(data, colx, coly, year){
    coul <- brewer.pal(length(data[[colx]]), "Greens") 
    barplot(height=data[[coly]],
            names=data[[colx]],
            col=coul,
            xlab="Month", ylab="Mean of Value", 
            main= paste("Value variation per month (", year, ")", sep=""))

    abline(h = data[which.max(data[[coly]]), ][[coly]], col="red")
}

plot_executive_value <- function(data, colx, coly, colz){
    coul <- brewer.pal(length(data[[colx]]), "Paired")
    matplot(data, type = c("b"),pch=1,col = coul) #plot
    # p <- ggplot(data,
    #             aes(x = data[[coly]], y = data[[colz]], group = data[[colx]], col = data[[colx]]),
    #             color=factor(data[[colx]])) + geom_line()
    # print(p)
}