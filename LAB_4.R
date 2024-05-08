# Set a fixed random seed for reproducibility
set.seed(10923)

# Number of students
num_students <- 500
# Enter code below:


# Simulate study hours (ranging from 1 to 20 hours)
study_hours <- sample(1:20, num_students, replace = TRUE)

# Simulate quiz scores (ranging from 0 to 100)
quiz_scores <- sample(0:100, num_students, replace = TRUE)

# Simulate forum participation (ranging from 0 to 50 posts)
forum_posts <- sample(0:50, num_students, replace = TRUE)

# Simulate previous grades (ranging from 0 to 100)
previous_grades <- sample(0:100, num_students, replace = TRUE)

# Simulate final grades (ranging from 0 to 100)
final_grades <- 0.3 * study_hours + 0.4 * quiz_scores + 0.2 * forum_posts + 0.1 * previous_grades + rnorm(num_students, mean = 0, sd = 5) + 25

# Create a data frame
student_data <- data.frame(StudyHours = study_hours, QuizScores = quiz_scores, ForumPosts = forum_posts, PreviousGrades = previous_grades, FinalGrades = final_grades)

# View the first few rows of the generated data
head(student_data)

# Explore the data
summary(student_data)
str(student_data)

# Modeling
# Splitting the data into training and testing sets (80% training, 20% testing)
set.seed(10923) # Set seed for reproducibility
sample_index <- sample(1:nrow(student_data), 0.8 * nrow(student_data))
train_data <- student_data[sample_index, ]
test_data <- student_data[-sample_index, ]

# Building a Linear Regression model using the train data and assign it to an object called model.
model <- lm(FinalGrades ~ StudyHours + QuizScores + ForumPosts + PreviousGrades, data = train_data)

# Making predictions on the test set
predictions <- predict(model, newdata = test_data)

# Evaluation metrics
# Compute the mean squared error and R-squared
mse <- mean((test_data$FinalGrades - predictions)^2)
rsquared <- summary(model)$r.squared

# Print evaluation metrics
cat("Mean Squared Error:", mse, "\n")
cat("R-squared:", rsquared, "\n")

# Model Accuracy based on Prediction Interval
# Get the predictions and prediction intervals
pred_int <- predict(model, newdata = test_data, interval = "prediction")

# Extract lower and upper bounds of the prediction interval
lower_bound <- pred_int[, "lwr"]
upper_bound <- pred_int[, "upr"]

# Actual values from the test data
actual_values <- test_data$FinalGrades

# Check if the actual values fall within the prediction interval
correct_predictions <- actual_values >= lower_bound & actual_values <= upper_bound

# Compute accuracy
accuracy <- sum(correct_predictions) / length(correct_predictions)

# Print accuracy
cat("Model Accuracy using Prediction Interval:", accuracy, "\n")

