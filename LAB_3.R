# Load necessary libraries
library(caret)
library(dplyr)
library(FactoMineR)
library(cluster)

# Read data file
data <- read.csv("C:/Users/MANIKANTA.N/Documents/MTH_665/oulad-students.csv", stringsAsFactors = FALSE)

data$final_result <- as.factor(data$final_result)

# Drop rows with missing values
data <- na.omit(data)

# Partition data - train (80%) & test (20%)
set.seed(12345)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.8, 0.2))
train <- data[ind == 1, ]
test <- data[ind == 2, ]

# Dimensionality Reduction using PCA
# Select numerical features for PCA
numeric_features <- select(train, -c(id_student, code_module, code_presentation, gender, region, highest_education, imd_band, age_band, num_of_prev_attempts, disability, module_presentation_length, final_result))

# Scale the numeric features
scaled_features <- scale(numeric_features)

# Perform PCA
pca_model <- prcomp(scaled_features, center = TRUE, scale. = TRUE)

# Extract principal components
pc_features <- pca_model$x

# Clustering using k-means
# Determine optimal number of clusters
wss <- numeric(10)
for (i in 1:10) {
  km <- kmeans(pc_features, centers = i, nstart = 10)
  wss[i] <- sum(km$withinss)
}

# Plot the within-cluster sum of squares
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Within-cluster sum of squares")

# Based on the plot, determine the optimal number of clusters (elbow point)
# Let's assume the optimal number of clusters is 3
num_clusters <- 3

# Perform k-means clustering
km <- kmeans(pc_features, centers = num_clusters, nstart = 10)

# Add cluster labels to the training data
train$cluster <- as.factor(km$cluster)

# Add cluster labels to the test data
test$cluster <- as.factor(kmeans(predict(pca_model, test), centers = num_clusters)$cluster)

# Logistic regression model with cluster variable
mymodel <- glm(final_result ~ . + cluster, data = train, family = 'binomial')
summary(mymodel)

# Prediction
p1 <- predict(mymodel, train, type = 'response')

# Misclassification error - train data
pred1 <- ifelse(p1 > 0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$final_result)
misclassification_error_train <- 1 - sum(diag(tab1))/sum(tab1)
cat("Misclassification error - train data:", misclassification_error_train, "\n")

# Prediction on test data
p2 <- predict(mymodel, test, type = 'response')
pred2 <- ifelse(p2 > 0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$final_result)
misclassification_error_test <- 1 - sum(diag(tab2))/sum(tab2)
cat("Misclassification error - test data:", misclassification_error_test, "\n")

# Goodness-of-fit test
with(mymodel, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
