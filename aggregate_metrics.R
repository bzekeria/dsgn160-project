# Aggregated Metrics Tables

## This script takes in our synthetic DoorDash data,
## calculates aggregated metrics, Customer Satisfaction Rating (CSR) and Delivery Time Variability (DTV),
## and creates two tables: one aggregated for all customers and one aggregated for each treatment level.

library(dplyr)

data <- read.csv("synthetic_experiment_data_group_yakovlev.csv")

# All customers
metrics_table_all_customers <- data %>%
  summarise(
    Group = "All Customers",
    CSR = mean(satisfaction),
    MeanDeliveryTime = mean(actualDeliveryTime),
    SD_DeliveryTime = sd(actualDeliveryTime),
    DTV = (sd(actualDeliveryTime) / mean(actualDeliveryTime)) * 100
  )

metrics_table_all_customers

# Treatment level
metrics_table_treatment <- data %>%
  group_by(Group) %>%
  summarise(
    CSR = mean(satisfaction),
    MeanDeliveryTime = mean(actualDeliveryTime),
    SD_DeliveryTime = sd(actualDeliveryTime),
    DTV = (sd(actualDeliveryTime) / mean(actualDeliveryTime)) * 100
  ) %>%
  mutate(Group = as.character(Group))  # Convert Group column to character type

metrics_table_treatment