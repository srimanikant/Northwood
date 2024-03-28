library(tidyverse)
library(caret)
library(ggplot2)

# Read data
data <- read.csv("C:/Users/Navya/Downloads/oulad-students.csv", stringsAsFactors = FALSE)

# Convert factors
data$code_module <- as.factor(data$code_module)
data$code_presentation <- as.factor(data$code_presentation)
data$gender <- as.factor(data$gender)
data$region <- as.factor(data$region)
data$highest_education <- as.factor(data$highest_education)
data$imd_band <- as.factor(data$imd_band)
data$age_band <- as.factor(data$age_band)
data$num_of_prev_attempts <- as.factor(data$num_of_prev_attempts)
data$disability <- as.factor(data$disability)
data$module_presentation_length <- as.factor(data$module_presentation_length)
data$date_registration <- as.factor(data$date_registration)
data$date_unregistration <- as.factor(data$date_unregistration)
data$final_result <- as.factor(data$final_result)

# Drop rows with missing values
data <- na.omit(data)

# Split data into train and test
set.seed(2995) # Equivalent to random_state=2995
train_index <- createDataPartition(data$final_result, p = 0.75, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Train the classification model (logistic regression)
model <- glm(final_result ~ ., data = train_data, family = binomial)

# Predict on test data
predictions <- predict(model, newdata = test_data, type = "response")

# Predict on test data
predictions <- ifelse(predictions > 0.5, "Pass", "Fail")

# Create confusion matrix
conf_matrix <- table(test_data$final_result, predictions)

# Convert confusion matrix to data frame
conf_df <- as.data.frame.matrix(conf_matrix)
conf_df <- cbind(Actual = rownames(conf_df), conf_df)
rownames(conf_df) <- NULL
conf_df <- tidyr::gather(conf_df, key = "Predicted", value = "Count", -Actual)

# Plotting using ggplot2
ggplot(conf_df, aes(x = Actual, y = Predicted, fill = Count)) +
  geom_tile(color = "black") +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(title = "Confusion Matrix",
       x = "Actual",
       y = "Predicted") +
  theme_minimal()

