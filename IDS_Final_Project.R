install.packages("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)
library(dplyr)
library(caret)
library(e1071)

########################################input CSV file####################################
########################################################################################

data <- read.csv("C:/Users/ungab/OneDrive/Desktop/cancer_patient.csv")

###################################################data Exploration############################
##############################################################################################
head(data, 5)

duplicates <- data[duplicated(data), ]
print(duplicates)
colSums(is.na(data))
dim(data)
variable_names <- names(data)
print(variable_names)
str(data)
summary(data)
print(data)
##################################Transformation######################################
###################################################################################

age_breaks <- c(14, 31, 61, 91)
age_labels <- c("Young", "Middle-aged", "Old")
data$Age <- cut(data$Age, breaks = age_breaks, labels = age_labels, include.lowest = TRUE)
print(data)
data$Gender <- factor(data$Gender, levels = c(1, 2), labels = c("Male", "Female"))


#################################data Visualization of Attributes##################################
###############################################################################################

ggplot(data, aes(x = Age)) +
  geom_bar(fill = 'skyblue', color = 'black') +
  labs(title = '  Age group of patients', x = 'Age', y = 'Count') +
  theme_minimal()

ggplot(data, aes(x = Level)) +
  geom_bar(fill = 'lightgreen', color = 'black') +
  labs(title = '  risk level for lung cancer', x = 'Level', y = 'Count') +
  theme_minimal()

ggplot(data, aes(x = Gender)) +
  geom_bar(fill = 'lightblue', color = 'black') +
  labs(title = '  Patient Gender', x = 'Gender', y = 'Count') +
  theme_minimal()


ggplot(data, aes(x = Genetic.Risk, fill = Level)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Genetic Risk of Lung Cancer according Lung Cancer level",
       x = "Genetic Risk of Lung Cancer Level 1-7",
       y = "Count") +
  theme_minimal()
  

ggplot(data, aes(x = Age, fill = Gender)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Bar Graph of Patients Age & Gender",
       x = "Age",
       y = "Count") +
  theme_minimal()


ggplot(data, aes(x = Smoking, fill = Age)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(title = "Bar Graph of Smoking Level & Age",
       x = "Smoking Level 1-8",
       y = "Count") +
  theme_minimal()

###############################Checking Actual frequency of Some Attributes####################################
#############################################################################################################
Age_Actual_Value_freq <- table(data$Age)
View(Age_Actual_Value_freq)

Gender_Actual_Value_freq <- table(data$Gender)
View(Gender_Actual_Value_freq)

Level_Actual_Value_freq <- table(data$Level)
View(Level_Actual_Value_freq)

ChestPain_Actual_Value_freq <- table(data$Chest.Pain)
View(ChestPain_Actual_Value_freq)

WeightLoss_Actual_Value_freq <- table(data$Weight.Loss)
View(WeightLoss_Actual_Value_freq)

AirPollution_Actual_Value_freq <- table(data$Air.Pollution)
View(AirPollution_Actual_Value_freq)

Smoking_Actual_Value_freq <- table(data$Smoking)
View(Smoking_Actual_Value_freq)

BalancedDiet_Actual_Value_freq <- table(data$Balanced.Diet)
View(BalancedDiet_Actual_Value_freq)

####################Perform the chi-squared test########################
#######################################################################
contingency_table <- table(data$Level, data$index)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)
data <- subset(data, select = -index)

contingency_table <- table(data$Level, data$Patient.Id)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)
data <- subset(data, select = -Patient.Id)

contingency_table <- table(data$Level, data$Age)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Gender)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Air.Pollution)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Alcohol.use)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Dust.Allergy)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$OccuPational.Hazards)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Genetic.Risk)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$chronic.Lung.Disease)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Balanced.Diet)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Obesity)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Smoking)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Passive.Smoker)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Chest.Pain)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Coughing.of.Blood)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Fatigue)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Weight.Loss)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Shortness.of.Breath)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Wheezing)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Swallowing.Difficulty)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Clubbing.of.Finger.Nails)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Frequent.Cold)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Dry.Cough)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

contingency_table <- table(data$Level, data$Snoring)
chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

###########################train & Test Model##########################
#######################################################################

set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train_data <- data[sample, ]
test_data <- data[!sample, ]

ggplot(test_data, aes(x = Level)) +
  geom_bar(fill = 'lightgreen', color = 'black') +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = 'black', size = 3) +
  labs(title = 'Risk Level for Lung Cancer', x = 'Level', y = 'Count') +
  theme_minimal()

ggplot(train_data, aes(x = Level)) +
  geom_bar(fill = 'lightblue', color = 'black') +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = 'black', size = 3) +
  labs(title = 'Risk Level for Lung Cancer', x = 'Level', y = 'Count') +
  theme_minimal()

response_variable <- "Level"
nb_model <- train(train_data[, -which(names(train_data) == response_variable)], 
                  train_data[[response_variable]], 
                  method = "nb")


predictions <- predict(nb_model, newdata = test_data[, -which(names(test_data) == response_variable)])
print(predictions)

test_data$Level <- factor(test_data$Level, levels = levels(predictions))
conf_matrix <- confusionMatrix(predictions, test_data[[response_variable]])
print(conf_matrix)

accuracy <- conf_matrix$overall["Accuracy"]
print(paste("Accuracy:", accuracy))

statistics_by_class <- conf_matrix$byClass
sensitivity <- statistics_by_class[, "Sensitivity"]
specificity <- statistics_by_class[, "Specificity"]
pos_pred_value <- statistics_by_class[, "Pos Pred Value"]
neg_pred_value <- statistics_by_class[, "Neg Pred Value"]

precision <- pos_pred_value * sensitivity / (pos_pred_value * sensitivity + (1 - specificity))
recall <- sensitivity
f1_score <- 2 * (precision * recall) / (precision + recall)
cat("Precision:", precision, "\n")
cat("Recall (Sensitivity):", recall, "\n")
cat("F1 Score:", f1_score, "\n")

ggplot(data = conf_df, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  geom_text(aes(label = sprintf("%d", Freq)), vjust = 1) +
  theme_minimal() +
  labs(x = "Actual", y = "Predicted", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#######################################
num_folds <- 10

test_accuracy_scores <- numeric(num_folds)
recall_scores <- numeric(num_folds)
precision_scores <- numeric(num_folds)
f1_scores <- numeric(num_folds)
confusion_matrices <- list()


for (i in 1:num_folds) {
  num_samples <- nrow(data)s
  test_size <- round(0.3 * num_samples)  # 30% for testing
  test_indices <- sample(1:num_samples, size = test_size, replace = FALSE)
  train_indices <- setdiff(1:num_samples, test_indices)
  
  train_data <- data[train_indices, ]
  test_data <- data[test_indices, ]
  
  nb_model <- train(train_data[, -which(names(train_data) == response_variable)], 
                    train_data[[response_variable]], 
                    method = "nb")

  test_predictions <- predict(nb_model, test_data[, -which(names(test_data) == response_variable)])
  
  test_accuracy_scores[i] <- sum(test_predictions == test_data[[response_variable]]) / length(test_predictions)

  cat("Fold", i, "Accuracy:", test_accuracy_scores[i], "\n")

  confusion_matrix <- table(test_predictions, test_data[[response_variable]])
  confusion_matrix_name <- paste("Confusion_Matrix_Fold_", i, sep="")
  confusion_matrices[[confusion_matrix_name]] <- confusion_matrix
  cat("Confusion Matrix for Fold", i, ":\n")
  print(confusion_matrix)
  
  recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  recall_scores[i] <- recall
  precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  precision_scores[i] <- precision
  f1 <- 2 * (precision * recall) / (precision + recall)
  f1_scores[i] <- f1

  cat("Fold", i, "Recall:", recall, "Precision:", precision, "F1 Score:", f1, "\n")
}

mean_test_accuracy <- mean(test_accuracy_scores)
mean_recall <- mean(recall_scores)
mean_precision <- mean(precision_scores)
mean_f1 <- mean(f1_scores)

cat("Mean Accuracy:", mean_test_accuracy, "\n")
cat("Mean Recall:", mean_recall, "\n")
cat("Mean Precision:", mean_precision, "\n")
cat("Mean F1 Score:", mean_f1, "\n")
