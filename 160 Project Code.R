##### DSGN 160 Project Code #####

setwd("/Users/arina/DSGN_160") 

install.packages("dplyr")
install.packages("ggplot2")
install.packages("sampling")
library(dplyr)
library(ggplot2)
library(sampling)

project_data <- read.csv("synthetic_experiment_data_group_yakovlev.csv")
