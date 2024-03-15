## Main Script
#---> Combines '160 Project Code.R' and 'statistical_inference.R'

## Install packages if not already installed
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("sampling")
# install.packages("lmtest")

library(dplyr)
library(ggplot2)
library(sampling)
library(lmtest)

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


# Statistical Inference

# same as project_data above though treatment values have been renamed, so to not effect the code below, refer to df
df <- read.csv("synthetic_experiment_data_group_yakovlev.csv")

## Anova

#### Since we're trying to understand how wait time affects customer satisfaction,
#### we'll conduct an ANOVA test to examine differences in customer satisfaction
#### among the control group and treatment groups

anova_result <- aov(satisfaction ~ Group, data = df)
summary(anova_result)

p_value <- summary(anova_result)[[1]]$"Pr(>F)"[1]
p_value

#### As shown above, the results indicate a statistically significant difference (in menas) overall.
#### However, it's essential to further investigate if customer ratings differ significantly
#### between all possible combinations of the treatment groups given.

## Tukey's HSD (Honestly Significant Difference) test 
## https://personal.utdallas.edu/~herve/abdi-HSD2010-pretty.pdf

tukey_result <- TukeyHSD(anova_result)

# Extract mean differences, p-values, lower bounds, and upper bounds
tukey_df <- data.frame(
  mean_diff = tukey_result$`Group`[, "diff"],
  p_value = tukey_result$`Group`[, "p adj"],
  lower_bound = tukey_result$`Group`[, "lwr"],
  upper_bound = tukey_result$`Group`[, "upr"]
)

tukey_df$stat_sig <- ifelse(tukey_df$p_value < 0.05, "Yes", "No")

#### From the results of Tukey's HSD test, we can conclude that treatments applied in groups 2 and 3 have led to 
#### statistically significant improvements in customer satisfaction compared to the control group. However, there 
#### are no significant differences in satisfaction levels between treatment groups themselves. This suggests that 
#### while the specific treatments in groups 2 and 3 are effective in enhancing customer satisfaction, they are 
#### similarly effective and don't show a significant advantage over each other. To understand which factors contribute
#### to customer satisfaction, we'll use linear regression. 

## Linear Regression

lm_model <- lm(satisfaction ~ AvgWaitTime + BufferTimePercentage + actualDeliveryTime + expectedDeliveryTime + cancellation, data = df)
summary(lm_model)

# Linear regression DF 
lm_coefficients <- summary(lm_model)$coefficients

lm_summary <- data.frame(
  estimate = lm_coefficients[, 1],
  std.Error = lm_coefficients[, 2],
  t_value = lm_coefficients[, 3],
  p_value = lm_coefficients[, 4]
)

lm_summary$sig_lvl <- ifelse(lm_summary$p_value < 0.05, "p < 0.05", ifelse(lm_summary$p_value < 0.1, "0.05 ≤ p < 0.1", ifelse(lm_summary$p_value < 0.5, "0.1 ≤ p < 0.5", "p ≥ 0.5")))

lm_summary

plot(lm_model)

#----> Results
#### As seen above, longer actual delivery times significantly reduce customer satisfaction (β = -0.0387, p < 0.001), 
#### indicating that for every additional minute of delivery time, customer satisfaction decreases by approximately 
#### 0.0387 units on average. Conversely, meeting or surpassing expected delivery times positively influences 
#### satisfaction (β = 0.0150, p = 0.0359), with every additional minute of expected delivery time associated with 
#### an increase in satisfaction by about 0.0150 units on average.
#### However, neither the average wait time nor buffer time demonstrate a statistically significant association with 
#### satisfaction. This suggests that variations in the average expected wait time per restaurant and artificially 
#### increasing the wait time do not significantly impact satisfaction levels. Similarly, order cancellations don't 
#### significantly influence satisfaction.
#----> Conclusion
#### In summary, while longer actual delivery times negatively affect satisfaction and meeting or 
#### exceeding expected delivery times positively impacts it, average wait time, buffer time, and order cancellations 
#### don't significantly influence satisfaction levels. 
#----> Recommendations
#### Therefore, optimizing actual delivery times to align with 
#### or surpass expected times is crucial for enhancing customer satisfaction in DoorDash's service model.

## Visualizations

tukey_df$stat_sig <- factor(tukey_df$stat_sig, levels = c("Yes", "No"))

tukey_plot <- ggplot(tukey_df, aes(x = c("T1 - C", "T2 - C", "T3 - C",
                                         "T2 - T1", "T3 - T1", "T3 - T2"), y = mean_diff, color = stat_sig)) +
  geom_point(size = 3, position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lower_bound, ymax = upper_bound),
                width = 0.2, position = position_dodge(width = 0.9)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Tukey's HSD: Mean Difference and 95% CI",
       subtitle = "Satisfaction Scores",
       y = "Mean Difference",
       x = "Comparison",
       color = "Statistical Significance") +
  scale_color_manual(values = c("#2ca02c", "#d62728"), labels = c("Yes", "No")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

tukey_plot

ggsave("tukey_plot.png", plot = tukey_plot)


#### The wider confidence intervals observed in some pairwise comparisons reflect uncertainty in estimating 
#### the mean differences between treatment groups. In our study, this uncertainty stems from variations in 
#### treatment effects across different groups of customers and their responses to the treatments. These 
#### wider intervals suggest that while there may be noticeable differences in customer satisfaction between 
#### certain treatment pairs, the exact magnitude of these differences is less precisely estimated due to factors 
#### such as sample variability (variation in the sampled data) or 
#### heterogeneous treatment effects (differences in how treatments affect different customer groups).
