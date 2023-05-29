# Load required libraries
library(randomForest)
library(tidyverse)
library(rpart)
library(rpart.plot)
library(e1071)
library(rpart)
library(rpart.plot)
library(dbscan)
library(cluster)
library(factoextra)

# Read the dataset
df <- read.csv("/Users/paperich/Documents/SDN_Parvathi/dataset_sdn.csv")

# Perform data cleaning tasks
# 1. Handling missing values
df <- na.omit(df)  # Remove rows with missing values
dataframe <- df
# 2. Handling outliers

# Calculate IQR to detect outliers and remove them:
Q1 <- quantile(df$bytecount, 0.25)
Q3 <- quantile(df$bytecount, 0.75)
IQR <- Q3 - Q1
df <- df[df$bytecount > (Q1 - 1.5 * IQR) & df$bytecount < (Q3 + 1.5 * IQR), ]  # Remove outliers

# 3. Handling categorical variables
df <- df %>%
  mutate(Protocol = as.factor(Protocol))  # Convert 'Protocol' to a factor variable

# 4. Normalizing or scaling features
df$bytecount <- (df$bytecount - min(df$bytecount)) / (max(df$bytecount) - min(df$bytecount))  # Min-max scaling
df$dur <- (df$dur - min(df$dur)) / (max(df$dur) - min(df$dur))  # Min-max scaling

# Split the dataset into training and testing sets
set.seed(123)
train_idx <- sample(1:nrow(df), nrow(df) * 0.8)  # 80% for training
train_df <- df[train_idx, ]
test_df <- df[-train_idx, ]

# Fit a random forest model
model <- randomForest(label ~ dt + switch + src + dst + pktcount + bytecount + dur + dur_nsec + tot_dur + flows + packetins + pktperflow + byteperflow + pktrate + Pairflow + Protocol + port_no + tx_bytes + rx_bytes + tx_kbps + rx_kbps + tot_kbps, data = train_df, importance = TRUE)

# Get feature importance scores
importance <- importance(model)

# Print feature importance scores
print(importance)

# Plot feature importance scores
varImpPlot(model)

df <- dataframe

# Normalizing or scaling features
df$bytecount <- (df$bytecount - min(df$bytecount)) / (max(df$bytecount) - min(df$bytecount))  # Min-max scaling
df$dur <- (df$dur - min(df$dur)) / (max(df$dur) - min(df$dur))  # Min-max scaling
df$packetins <- (df$packetins - min(df$packetins)) / (max(df$packetins) - min(df$packetins))  # Min-max scaling
df$byteperflow <- (df$byteperflow - min(df$byteperflow)) / (max(df$byteperflow) - min(df$byteperflow))  # Min-max scaling
df$pktcount <- (df$pktcount - min(df$pktcount)) / (max(df$pktcount) - min(df$pktcount))  # Min-max scaling
df$pktperflow <- (df$pktperflow - min(df$pktperflow)) / (max(df$pktperflow) - min(df$pktperflow))  # Min-max scaling
df$pktrate <- (df$pktrate - min(df$pktrate)) / (max(df$pktrate) - min(df$pktrate))  # Min-max scaling

# Handling categorical variables
one_hot_Protocol <- model.matrix(~ Protocol - 1, data = df)
one_hot_src <- model.matrix(~ src - 1, data = df)

# Combine the one-hot encoded variables with the original data
df <- cbind(df, one_hot_Protocol)
df <- cbind(df, one_hot_src)

# Perform one-hot encoding for Protocol and src variables
one_hot_Protocol <- model.matrix(~ Protocol - 1, data = df)
one_hot_src <- model.matrix(~ src - 1, data = df)

# Combine the one-hot encoded variables with the original data
df <- cbind(df, one_hot_Protocol)
df <- cbind(df, one_hot_src)

# Update the feature matrix df
df <- df[, c("pktcount", "packetins", "byteperflow", "pktperflow", "bytecount", "dt", "pktrate", "dur_nsec","label")]
df <- cbind(df, one_hot_Protocol)
df <- cbind(df, one_hot_src)

# Split the dataset into training and testing sets
set.seed(123)
train_idx <- sample(1:nrow(df), nrow(df) * 0.8)  # 80% for training
train_df <- df[train_idx, ]
test_df <- df[-train_idx, ]

# Fit logistic regression model
regression_model <- glm(label ~ ., data = train_df, family = "binomial")

# Make predictions on test data
regression_model_predictions <- predict(regression_model, newdata = test_df, type = "response")
regression_model_predicted_labels <- ifelse(regression_model_predictions > 0.5, 1, 0)

# Evaluate model performance
regression_model_confusion_matrix <- table(test_df$label, regression_model_predicted_labels)
regression_model_accuracy <- sum(diag(regression_model_confusion_matrix)) / sum(regression_model_confusion_matrix) * 100
regression_model_precision <- sum(regression_model_predicted_labels[test_df$label == 1] == 1) / sum(regression_model_predicted_labels == 1) * 100
regression_model_recall <- sum(regression_model_predicted_labels[test_df$label == 1] == 1) / sum(test_df$label == 1) * 100
regression_model_f1_score <-  2 * (regression_model_precision * regression_model_recall) / (regression_model_precision + regression_model_recall)

cat("Accuracy: ", regression_model_accuracy, "\n")
cat("Precision: ", regression_model_precision, "\n")
cat("Recall: ", regression_model_recall, "\n")
cat("F1 Score: ", regression_model_f1_score, "\n")


# Fit decision tree model
decision_tree_model <- rpart(label ~ pktcount + packetins + byteperflow + pktperflow + bytecount, data = train_df)

# Visualize the decision tree
rpart.plot(decision_tree_model)

# Make predictions on test data
decision_tree_predictions <- predict(decision_tree_model, newdata = test_df)

# Convert predicted values to binary class labels
decision_tree_predicted_labels <- ifelse(decision_tree_predictions > 0.5, 1, 0)

# Calculate accuracy
decision_tree_accuracy <- sum(decision_tree_predicted_labels == test_df$label) / nrow(test_df) * 100

# Calculate precision
decision_tree_precision <- sum(decision_tree_predicted_labels[test_df$label == 1] == 1) / sum(decision_tree_predicted_labels == 1) * 100

# Calculate recall
decision_tree_recall <- sum(decision_tree_predicted_labels[test_df$label == 1] == 1) / sum(test_df$label == 1) * 100

# Calculate F1 score
decision_tree_f1_score <- 2 * (decision_tree_precision * decision_tree_recall) / (decision_tree_precision + decision_tree_recall)

cat("Accuracy: ", decision_tree_accuracy, "\n")
cat("Precision: ", decision_tree_precision, "\n")
cat("Recall: ", decision_tree_recall, "\n")
cat("F1 Score: ", decision_tree_f1_score, "\n")


# Train the SVM model
svm_model <- svm(label ~ ., data = train_df, kernel = "radial", cost = 1)

# Make predictions on test data
svm_predictions <- predict(svm_model, test_df)

svm_predicted_labels <- ifelse(svm_predictions > 0.5, 1, 0)

# Calculate accuracy
svm_accuracy <- sum(svm_predicted_labels == test_df$label) / nrow(test_df) * 100
cat("Accuracy of SVM model:", svm_accuracy, "%\n")

# Calculate precision
svm_precision <- sum(svm_predicted_labels[test_df$label == 1] == 1) / sum(svm_predicted_labels == 1) * 100

# Calculate recall
svm_recall <- sum(svm_predicted_labels[test_df$label == 1] == 1) / sum(test_df$label == 1) * 100

# Calculate F1 score
svm_f1_score <- 2 * (svm_precision * svm_recall) / (svm_precision + svm_recall)

df <- dataframe
dataset_with_label <- df[,c('switch','src','dst','Protocol','port_no','label')]
# loop over each column in the data frame
for (i in 1:ncol(df)) {
  # check if the column is numeric or integer
  if (is.numeric(df[,i]) || is.integer(df[,i])) {
    # check each value in the column for finiteness and replace infinite values with a large finite value
    df[!is.finite(df[,i]), i] <- 1e+10
  }
}

# print the data frame
print(df)

# convert character columns to factors
df <- sapply(df, function(x) {
  if (is.character(x)) {
    factor(x)
  } else {
    x
  }
})

# Fitting K-Means clustering Model
# to training dataset
set.seed(240) # Setting seed
kmeans <- kmeans(df, centers = 2, nstart = 20)
kmeans

kmeans$cluster
kmeans$centers
p1 <- fviz_cluster(kmeans, geom = "point", data = df) + ggtitle("k = 2")
p1

dataset_with_label$label <- ifelse(dataset_with_label$label == 0, 1, 2)
table(dataset_with_label$label, kmeans$cluster)

# Convert dataset_with_label to vector
dataset_with_label <- as.vector(dataset_with_label$label)

# Compute accuracy
kmeans_accuracy <- sum(kmeans$cluster == dataset_with_label) / length(dataset_with_label) * 100

# Create confusion matrix
kmeans_confusion_matrix <- table(dataset_with_label, kmeans$cluster)

# Calculate precision, recall, and F1-score as percentages
kmeans_precision <- diag(kmeans_confusion_matrix) / colSums(kmeans_confusion_matrix) * 100
kmeans_recall <- diag(kmeans_confusion_matrix) / rowSums(kmeans_confusion_matrix) * 100
kmeans_f1_score <- 2 * (kmeans_precision * kmeans_recall) / (kmeans_precision + kmeans_recall)

# Extract the values for label 1
label_of_interest <- 2
kmeans_precision_label_2 <- kmeans_precision[label_of_interest]
kmeans_recall_label_2 <- kmeans_recall[label_of_interest]
kmeans_f1_score_label_2 <- kmeans_f1_score[label_of_interest]

# Print the results
cat("Precision for label", label_of_interest, ":", kmeans_precision_label_2, "\n")
cat("Recall for label", label_of_interest, ":", kmeans_recall_label_2, "\n")
cat("F1-score for label", label_of_interest, ":", kmeans_f1_score_label_2, "\n")


# Create a data frame with performance metrics for different models
performance_data <- data.frame(
  Model = c("Logistic Regression", "Decision Tree", "SVM", "K-Means", "Random Forest"),
  Accuracy = c(regression_model_accuracy, decision_tree_accuracy, svm_accuracy, kmeans_accuracy, randomForest_accuracy),
  Precision = c(regression_model_precision, decision_tree_precision, svm_precision, kmeans_precision_label_2,randomForest_precision),
  Recall = c(regression_model_recall, decision_tree_recall, svm_recall, kmeans_recall_label_2, randomForest_recall),
  F1_Score = c(regression_model_f1_score, decision_tree_f1_score, svm_f1_score, kmeans_f1_score_label_2,randomForest_f1_score)
)

# Reshape the data frame to long format for easier plotting
performance_data_long <- tidyr::gather(performance_data, Metric, Value, -Model)

# Create the bar chart
bar_chart <- ggplot(performance_data_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Performance Comparison of Different Models",
       x = "Model",
       y = "Value") +
  scale_fill_discrete(name = "Metric")

# Show the bar chart
print(bar_chart)
hybrid <- ifelse(decision_tree_predicted_labels == 1 & svm_predicted_labels == 1, 1, 0)
hybrid_accuracy <- sum(hybrid == test_df$label)/nrow(test_df)

