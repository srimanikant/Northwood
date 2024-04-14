# Read data file
data <- read.csv("C:/Users/MANIKANTA.N/Documents/MTH_665/oulad-students.csv", stringsAsFactors = FALSE)
str(data)
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
data$final_result <- as.factor(data$final_result)

# Drop rows with missing values
data <- na.omit(data)

# Partition data - train (80%) & test (20%)
set.seed(12345)
ind <- sample(2, nrow(data), replace = T, prob = c(0.8, 0.2))
train <- data[ind==1,]
test <- data[ind==2,]

# Logistic regression model
mymodel <- glm(final_result ~ ., data = train, family = 'binomial')
summary(mymodel)

# Prediction
p1 <- predict(mymodel, train, type = 'response')
head(p1)
head(train)

# Misclassification error - train data
pred1 <- ifelse(p1>0.5, 1, 0)
tab1 <- table(Predicted = pred1, Actual = train$final_result)
tab1
1 - sum(diag(tab1))/sum(tab1)

# Misclassification error - test data
p2 <- predict(mymodel, test, type = 'response')
pred2 <- ifelse(p2>0.5, 1, 0)
tab2 <- table(Predicted = pred2, Actual = test$final_result)
tab2
1 - sum(diag(tab2))/sum(tab2)

# Goodness-of-fit test
with(mymodel, pchisq(null.deviance - deviance, df.null-df.residual, lower.tail = F))