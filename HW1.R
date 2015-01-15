#' ---
#' title: "BIOST 578 Homework 1"
#' author: "Marea Cobb"
#' date: "January 15, 2015"
#' reviewer: 
#' ---


#' Loads specific libraries 
library(ggplot2)
library(RColorBrewer)
library(knitr)

#' Creates the R markdown files.
spin(report = TRUE, hair = "HW1.R", format = "Rmd")
file.rename("HW1.md", "HW1.Rmd")

colors <- brewer.pal(9, "Set3")

#' Loads the iris data
data(iris)

#' Creates a new column for flower_id for each entry.
iris$flower_id <- rownames(iris)

#' Uses the default melt method to rearrange the data for easier classification.
iris_melted <- melt(iris)

#' Splits the variable at the "." to get the value name (length/width and flower type)
split_variable <- strsplit(as.character(iris_melted$variable), split="\\.")
#' Creates the variable "flower_part" from the first value in split_variable and adds it to the table
iris_melted$flower_part <- sapply(split_variable, "[", 1)
#' Creates the variable "measurement_type" from the second value in split_variable and adds it to the table
iris_melted$measurement_type <- sapply(split_variable, "[", 2)
#' Removes variable from the table as we no longer need it
iris_melted$variable <- NULL

#' Reshapes the data so iris$width and iris$length are now separte columns in the table 
iris_cast <- dcast(iris_melted, formula=flower_id+Species+flower_part~measurement_type)


#' Creates a plot to 
ggplot(data=iris_cast, colour=colors, aes(x=Width, y=Length, color=Species))+ # Add points and use free scales in the facet
  geom_point()+facet_grid(Species~flower_part, scale="free")+
  #' Add a regression line
  geom_smooth(method="lm")+
  #' Use the black/white theme and increase the font size
  theme(plot.background = element_rect(colour="blue"))

dev.off()
