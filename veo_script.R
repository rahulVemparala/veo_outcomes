# Load necessary packages
if (!require("readr")) install.packages("readr")
library(readr)
library(readxl)
library(tidyverse)

# Load data tables
veo_occ_pay <- read_excel("veo_occ_pay_grade.xlsx")
veo_occ_ind <- read_excel("veo_occ_ind.xlsx")
veo_edu <- read_excel("veo_edu.xlsx")


# Drop rows with missing values
veo_edu <- na.omit(veo_edu)
veo_occ_pay <- na.omit(veo_occ_pay)
veo_occ_ind <- na.omit(veo_occ_ind)

# Summary of data
str(veo_occ_pay)
str(veo_edu)
str(veo_occ_ind)

## Exclude non employment earnings from analysis.
veo_edu <- veo_edu[, !(names(veo_edu) %in% c("y1_nonemp", "y5_nonemp",
                                             "y10_nonemp"))]
veo_occ_pay <- veo_occ_pay[, !(names(veo_occ_pay) %in% c("y1_nonemp", "y5_nonemp",
                                                         "y10_nonemp"))]

# Define the columns to convert to numeric
columns_to_convert <- c("y5_p25_earnings", "y10_p25_earnings", 
                        "y5_p50_earnings", "y10_p50_earnings", 
                        "y5_p75_earnings", "y10_p75_earnings",
                        "y5_emp", "y10_emp")

# Convert the specified columns to numeric
veo_edu[, columns_to_convert] <- lapply(veo_edu[, columns_to_convert], as.numeric)
veo_occ_pay[, columns_to_convert] <- lapply(veo_occ_pay[, columns_to_convert],
                                            as.numeric)
veo_occ_ind[, columns_to_convert] <- lapply(veo_occ_ind[, columns_to_convert],
                                            as.numeric)

# Function to check for non-numeric values in a column
check_non_numeric <- function(column) {
  !grepl("^\\d+$", column)
}

# Apply the function to each column
non_numeric_values <- sapply(veo_edu[, columns_to_convert], check_non_numeric)
non_numeric_values <- sapply(veo_occ_pay[, columns_to_convert], check_non_numeric)
non_numeric_values <- sapply(veo_occ_ind[, columns_to_convert], check_non_numeric)


# Print rows with non-numeric values before removal
#print(veo_edu[non_numeric_rows, ])

# Remove rows with non-numeric values  ( omitting rows
#veo_edu <- veo_edu[!non_numeric_rows, ]


# Remove rows with status flag -1 for earnings and emp columns
veo_edu <- veo_edu[veo_edu$status_y1_emp != -1 &
                           veo_edu$status_y5_emp != -1 
                         & veo_edu$status_y10_emp != -1, ]

veo_occ_pay <- veo_occ_pay[veo_occ_pay$status_y1_emp != -1 &
                             veo_occ_pay$status_y5_emp != -1 
                           & veo_occ_pay$status_y10_emp != -1, ]
veo_occ_ind <- veo_occ_ind[veo_occ_ind$status_y1_emp != -1 &
                             veo_occ_ind$status_y5_emp != -1 
                           & veo_occ_ind$status_y10_emp != -1, ]



# Impute missing values in y5_emp column with its mean
veo_occ_pay$y5_emp <- ifelse(is.na(veo_occ_pay$y5_emp),
                             mean(veo_occ_pay$y5_emp, na.rm = TRUE),
                             veo_occ_pay$y5_emp)

veo_occ_ind$y5_emp <- ifelse(is.na(veo_occ_ind$y5_emp),
                             mean(veo_occ_ind$y5_emp, na.rm = TRUE),
                             veo_occ_ind$y5_emp)



veo_occ_pay$y10_emp <- ifelse(is.na(veo_occ_pay$y10_emp),
                              mean(veo_occ_pay$y10_emp, na.rm = TRUE),
                              veo_occ_pay$y10_emp)

veo_occ_ind$y10_emp <- ifelse(is.na(veo_occ_ind$y10_emp),
                              mean(veo_occ_ind$y10_emp, na.rm = TRUE),
                              veo_occ_ind$y10_emp)

sum(is.na(veo_occ_pay$y5_emp))  
sum(is.na(veo_occ_pay$y10_emp)) 


# Summary of data
str(veo_occ_pay)
str(veo_edu)



# EDA 
library(ggplot2)
library(scales)

# Plotting with formatted y-axis labels as currency
ggplot(veo_edu, aes(x = label_education, y = y1_emp)) +
  geom_boxplot() +
  labs(x = "Education Level", y = "Earnings in First Year of Cohort") +
  scale_y_continuous(labels = scales::dollar_format()) 



ggplot(veo_occ_ind, aes(x = label_dod_occ_code, y = y5_emp)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::dollar) + # Format y-axis as currency
  labs(x = "Occupation Code", y = "Earnings (USD)", 
       title = "Distribution of Earnings by Occupation Code")


ggplot(veo_occ_ind, aes(x = label_industry, y = y5_emp)) +
  stat_summary(fun = mean, geom = "bar") +
  scale_y_continuous(labels = scales::dollar) + # Format y-axis as currency
  labs(x = "Industry", y = "Mean Earnings (USD)", title = "Mean Earnings by Industry") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

ggplot(veo_occ_ind, aes(x = cohort_years, y = y5_emp)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) + # Format y-axis as currency
  labs(x = "Years of Experience", y = "Earnings (USD)", 
       title = "Earnings vs. Years of Experience")



# multi colinearity check 
library(car)

# Compute VIF for each predictor variable
vif_values_y1_emp <- vif(lm(y1_emp ~ label_cohort + label_dod_occ_code + label_paygrade, data = veo_occ_pay))
vif_values_y5_emp <- vif(lm(y5_emp ~ label_cohort + label_dod_occ_code + label_paygrade, data = veo_occ_pay))
vif_values_y10_emp <- vif(lm(y10_emp ~ label_cohort + label_dod_occ_code + label_paygrade, data = veo_occ_pay))

# Print VIF values
print(vif_values_y1_emp)
print(vif_values_y5_emp)
print(vif_values_y10_emp)


# EDA

# Plot observed vs. predicted values by education level
ggplot(data = data.frame(predicted, veo_edu$y1_emp, education = veo_edu$education), aes(x = predicted, y = veo_edu$y1_emp)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Predicted Earnings", y = "Observed Earnings", title = "Observed vs. Predicted Earnings") +
  facet_wrap(~education, scales = "free") +
  theme_minimal()



# Plot observed vs. predicted values by education level
ggplot(data = data.frame(predicted, veo_edu$y1_emp, education = veo_edu$education), aes(x = predicted, y = veo_edu$y1_emp, color = education)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(x = "Predicted Earnings", y = "Observed Earnings", title = "Observed vs. Predicted Earnings by Education Level") +
  theme_minimal()



# Grouped Boxplot of y5_emp by cohort_years and paygrade_level with earnings displayed without exponential notation
ggplot(veo_occ_pay, aes(x = factor(label_dod_occ_code), y = y1_p50_earnings, 
                        fill = paygrade_level)) +
  geom_boxplot() +
  labs(x = "occupation", y = "Earnings in First Year of Cohort", title = "Boxplot of Earnings by Cohort Years and Paygrade Level (First Year)") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::comma)  # Display earnings without exponential notation


# Convert y10_emp to numeric
veo_occ_pay$y10_emp <- as.numeric(as.character(veo_occ_pay$y10_emp))

# Grouped Boxplot of y10_emp by cohort_years and paygrade_level with earnings displayed without exponential notation
ggplot(veo_occ_pay, aes(x = factor(cohort_years), y = y10_emp, fill = paygrade_level)) +
  geom_boxplot() +
  labs(x = "Cohort Years", y = "Earnings in Tenth Year of Cohort", title = "Boxplot of Earnings by Cohort Years and Paygrade Level (Tenth Year)") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::comma)  # Display earnings without exponential notation





# Model building
# Linear regression model to predict y1_emp based on education
model_occ_pay_lm <- lm(y5_emp ~ label_dod_occ_code + cohort_years + label_paygrade,
            data = veo_occ_pay)

# Model summary
summary(model_occ_pay_lm)


# Check linearity assumption
plot(model_occ_pay_lm$fitted.values, model_occ_pay_lm$residuals, xlab = "Fitted Values", 
     ylab = "Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")  # Add horizontal line at y = 0

# Check independence of errors assumption
plot(model_occ_pay_lm$residuals ~ veo_occ_pay$label_paygrade)
abline(h = 0, col = "red")  # Add horizontal line at y = 0

# Check homoscedasticity assumption
plot(model_occ_pay_lm$fitted.values, abs(model_occ_pay_lm$residuals), 
     xlab = "Fitted Values", ylab = "Absolute Residuals", main = "Residuals vs Fitted")
abline(h = 0, col = "red")  # Add horizontal line at y = 0

# Check normality of residuals assumption
hist(model_occ_pay_lm$residuals, breaks = 20, main = "Histogram of Residuals")
qqnorm(model_occ_pay_lm$residuals)
qqline(model_occ_pay_lm$residuals)






# multi target reg model : veo_occ_pay

# Fit the multiple target regression model
model <- lm(cbind(y1_emp, y5_emp, y10_emp) ~ label_cohort +
              label_dod_occ_code + label_paygrade, data = veo_occ_pay)

# View the summary of the model
summary(model)


# OLS assumptions:
# Linearity check: Residuals vs Fitted Values plot
plot(fitted(model), residuals(model), xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values", col = "blue")
abline(h = 0, col = "red")  # Add horizontal line at y = 0


# Normality check: QQ plot
qqnorm(residuals(model))
qqline(residuals(model), col = 2)  # Add reference line


# Homoscedasticity check: Residuals vs Fitted Values plot
plot(fitted(model), residuals(model), xlab = "Fitted Values", ylab = "Residuals",
     main = "Residuals vs Fitted Values (Homoscedasticity Check)", col = "blue")
abline(h = 0, col = "red")  # Add horizontal line at y = 0






# model 3: random forest:
# Load the required library
library(randomForest)

# Fit the Random Forest model with y5_emp as the target
rf_model_y5_emp <- randomForest(y5_emp ~ label_cohort + 
                                  label_dod_occ_code + label_paygrade,
                                data = veo_occ_pay)


# Fit the Random Forest model for y5_emp
rf_model_y5_emp <- randomForest(y5_emp ~ label_cohort + 
                                  label_dod_occ_code + 
                                  label_paygrade, data = veo_occ_pay)


# 1. Variable Importance Plot
var_importance <- importance(rf_model_y5_emp)
plot(var_importance)

# 2. Partial Dependence Plot (for label_cohort)
library(pdp)

partialPlot(rf_model_y5_emp, pred.var = "label_cohort", train = veo_occ_pay)

# 3. Cross-validation
library(caret)

cv_results <- train(y5_emp ~ label_cohort + label_dod_occ_code + label_paygrade, data = veo_occ_pay,
                    method = "rf", trControl = trainControl(method = "cv", number = 10))

print(cv_results)

# 4. Prediction Performance Metrics (RMSE)
predicted_y5_emp <- predict(rf_model_y5_emp, newdata = veo_occ_pay)
actual_y5_emp <- veo_occ_pay$y5_emp
rmse_y5_emp <- sqrt(mean((predicted_y5_emp - actual_y5_emp)^2))

# Print RMSE
print(rmse_y5_emp)


# Fit the Random Forest model with optimal hyperparameters
rf_model_optimal <- randomForest(y5_emp ~ label_cohort + label_dod_occ_code + label_paygrade, 
                                 data = veo_occ_pay,
                                 mtry = 2)

# Summary of the model
print(rf_model_optimal)

varImpPlot(rf_model_optimal)
axis(side = 1, at = log(1:length(importance(rf_model_optimal))), 
     labels = colnames(veo_occ_pay)[-which(names(veo_occ_pay) == "y5_emp")])  # Logarithmic scale



# model 3: linear model with interaction variable:
# Create the interaction variable
veo_occ_pay$interaction <- interaction(veo_occ_pay$label_cohort, 
                                       veo_occ_pay$label_dod_occ_code)

# Fit the linear regression model with interaction term
model_interaction <- lm(y5_emp ~ label_cohort * label_dod_occ_code +
                          label_paygrade, data = veo_occ_pay)

# View the summary of the model
summary(model_interaction)

# Perform quality checks
# 1. Linearity check
plot(model_interaction)

# 2. Normality of Residuals
hist(residuals(model_interaction))

# 3. Homoscedasticity
plot(fitted(model_interaction), residuals(model_interaction))

# 4. Points Check (for any influential points)
# This can be done using various diagnostic plots like Cook's distance, leverage plot, etc.

# 5. Cross-validation (if applicable)
# For example, 10-fold cross-validation
cv_error_interaction <- cv.glm(veo_occ_pay, model_interaction, K = 10)
plot(cv_error_interaction)

# 6. Prediction Performance Metrics (if applicable)
# For regression models, you can calculate metrics like RMSE or MAE
predicted_interaction <- predict(model_interaction, newdata = veo_occ_pay)
actual_interaction <- veo_occ_pay$y5_emp
rmse_interaction <- sqrt(mean((predicted_interaction - actual_interaction)^2))

# Print RMSE
print(rmse_interaction)



# Load necessary library
library(stargazer)


# Specify the models to compare
models <- list(
  "Linear with Interaction" = model_interaction,
  "Linear Model" = model_occ_pay_lm
)

# Compare the models using stargazer
stargazer(models)
# Print comparison table
print(summary_stats)



models <- list(
  "Random Forest" = rf_model_optimal,
  "Linear with Interaction" = model_interaction,
  "Linear Model" = model_occ_pay_lm
)

# Extract the model objects from the list
model_objects <- sapply(models, function(x) x)

# Compare the models using stargazer
stargazer(model_objects)


# Get performance metrics using caret
library(caret)
model_results <- caretList(rf_model_optimal, model_interaction, model_occ_pay_lm)
performance(model_results)  # Shows performance metrics for all models



# stargazer
library(stargazer)
stargazer( model_occ_pay_lm, model_interaction,   type = "text")







