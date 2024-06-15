# PRODIGY_DS_03 

# TASK 3

# Install necessary libraries
install.packages("readxl", dependencies = TRUE)

install.packages("tidyverse", dependencies = TRUE)

install.packages("caret", dependencies = TRUE)

install.packages("rpart", dependencies = TRUE)

install.packages("rpart.plot", dependencies = TRUE)

# Load the libraries
library(readxl)

library(tidyverse)

library(caret)

library(rpart)

library(rpart.plot)

library(dplyr)

# Load the dataset
bank_data <- read_excel("C:/Users/GD DHANUSH/Downloads/bank-full.xlsx")

View(bank_data)

# Convert categorical variables to factors
bank_data <- bank_data %>%
  mutate_if(is.character, as.factor)

# Check for missing values
sum(is.na(bank_data))

# View the structure of the dataset
str(bank_data) 

# Set seed for reproducibility
set.seed(42)

# Create training and testing sets
trainIndex <- createDataPartition(bank_data$y, p = .7,list = FALSE, times = 1)
                                  
bank_train <- bank_data[ trainIndex,]

bank_test  <- bank_data[-trainIndex,]

# View the dimensions of the training and testing sets
dim(bank_train)

dim(bank_test)

# Train the decision tree model
model <- rpart(y ~ ., data = bank_train, method = "class")


# Plot the decision tree
rpart.plot(model, type = 2, extra = 104)


# Make predictions on the test set
predictions <- predict(model, bank_test, type = "class")

# Confusion matrix
conf_matrix <- confusionMatrix(predictions, bank_test$y)

# Print the confusion matrix
print(conf_matrix)


# Calculate additional metrics
accuracy <- conf_matrix$overall['Accuracy']

precision <- conf_matrix$byClass['Pos Pred Value']

recall <- conf_matrix$byClass['Sensitivity']

f1 <- 2 * ((precision * recall) / (precision + recall))

cat("Accuracy: ", accuracy, "\n")

cat("Precision: ", precision, "\n")

cat("Recall: ", recall, "\n")

cat("F1 Score: ", f1, "\n")









<img width="879" alt="image" src="https://github.com/DhanushCU/PRODIGY_DS_03/assets/159162806/58c050f9-79df-4dbd-9410-6d26927cc6c0">
