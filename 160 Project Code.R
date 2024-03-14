##### DSGN 160 Project Code #####

install.packages("dplyr")
install.packages("ggplot2")
install.packages("sampling")
library(dplyr)
library(ggplot2)
library(sampling)

project_data <- read.csv("synthetic_experiment_data_group_yakovlev.csv")

project_data$Group[project_data$Group=="control"] <- "0"
project_data$Group[project_data$Group=="treatment 1"] <- "1"
project_data$Group[project_data$Group=="treatment 2"] <- "2"
project_data$Group[project_data$Group=="treatment 3"] <- "3"

head(project_data)