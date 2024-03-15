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

project_data_str <- project_data %>% mutate(Group = case_when
    (Group == 0 ~ "control", Group == 1 ~ "5%", Group == 2 ~ "10%", Group == 3 ~ "15%"))
satisfaction_bytreatment <- ggplot(project_data_str, aes(x = reorder(Group, satisfaction), y = satisfaction)) + 
  geom_boxplot()
satisfaction_bytreatment
ggsave("satisfaction_boxplot.png", plot = satisfaction_bytreatment)