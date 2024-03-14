
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lmtest")
install.packages("MASS")


library(dplyr)
library(ggplot2)
library(lmtest)
library(MASS) # for robust regression

# Load Data
df <- read.csv("synthetic_experiment_data_group_yakovlev.csv")

# Statistical Inference

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

tukey_df <- data.frame(mean_diff = tukey_result$`Group`[, "diff"], p_value = tukey_result$`Group`[, "p adj"])
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

## Test for heteroscedasticity using BP test (Breusch and Pagan)
## https://www.statology.org/breusch-pagan-test/

hetero_test <- bptest(lm_model)
if (hetero_test$p.value < 0.05) {
  print("Heteroscedasticity detected")
}

#### Since the heteroscedasticity is detected, we need to implement a robust regression method.


## Robust Regression using Huber M-estimation
## https://stats.oarc.ucla.edu/r/dae/robust-regression/

lm_robust_model <- rlm(satisfaction ~ AvgWaitTime + BufferTimePercentage + actualDeliveryTime + expectedDeliveryTime + cancellation, data = df)

lm_robust_coefficients <- summary(lm_robust_model)$coefficients

lm_robust_summary <- data.frame(
  estimate = lm_robust_coefficients[, 1],
  std.Error = lm_robust_coefficients[, 2],
  t_value = lm_robust_coefficients[, 3]
)

deg_frdm_robust <- length(lm_robust$coefficients) - 1

lm_robust_summary$p_value <- pt(abs(lm_robust_summary$t_value), df = deg_frdm_robust, lower.tail = FALSE) * 2

lm_robust_summary$sig_lvl <- ifelse(lm_robust_summary$p_value < 0.05, "p < 0.05", ifelse(lm_robust_summary$p_value < 0.1, "0.05 ≤ p < 0.1", ifelse(lm_robust_summary$p_value < 0.5, "0.1 ≤ p < 0.5", "p ≥ 0.5")))

lm_robust_summary

#### The robust regression analysis confirms the significance of longer actual delivery times on 
#### customer satisfaction (β = -0.0370, p < 0.001). While the initial p-value for expected delivery 
#### times was 0.036, indicating potential significance, it's now found to be 0.072, suggesting 
#### inconclusive evidence in this context. Moreover, factors like average wait time, 
#### buffer time percentage, and order cancellation remain insignificantly associated with satisfaction 
#### levels. 



