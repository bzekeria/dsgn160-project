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

# 1. Bar plot comparing average cancellation to buffer times
#   Calculate average cancellation rate for each buffer time
avg_cancel_buffer <- aggregate(cancellation ~ BufferTimePercentage, data = project_data, FUN = mean)

ggplot(data = avg_cancel_buffer, aes(x = BufferTimePercentage, y = cancellation)) +
  geom_bar(stat = "identity", fill = "skyblue")

# 2. Avg satisfaction between buffer groups
#   Calculate average satisfaction rate for each buffer time
avg_satisfaction_buffer <- aggregate(satisfaction ~ BufferTimePercentage, data = project_data, FUN = mean)

ggplot(data = avg_satisfaction_buffer, aes(x = BufferTimePercentage, y = satisfaction)) +
  geom_bar(stat = "identity", fill = "sandybrown")

# 3. Barplot showing customer's frequency and satisfaction
# Returns new dataframe grouped by customerID, calculates frequency of each customer
customer_frequency <- project_data %>%
  group_by(customerID) %>%
  summarise(Frequency = n())

# Returns new dataframe where each customerID from customer_frequency has information from original data frame
customer_data <- inner_join(customer_frequency, project_data, by = "customerID")

average_satisfaction <- customer_data %>%
  group_by(Frequency) %>%
  summarise(AverageSatisfaction = mean(satisfaction))

ggplot(data = average_satisfaction, aes(x = Frequency, y = AverageSatisfaction)) +
  geom_bar(stat = "identity", fill = "coral")
