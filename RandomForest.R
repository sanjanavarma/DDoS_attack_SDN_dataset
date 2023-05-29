# Load required libraries
library(randomForest)

# Read the dataset
df <- read.csv("/Users/paperich/Documents/SDN_Parvathi/dataset_sdn.csv")

# Perform data cleaning tasks
# 1. Handling missing values
df <- na.omit(df)  # Remove rows with missing values

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
model <- randomForest(label ~ pktcount + packetins + byteperflow + pktperflow + bytecount, data = train_df, importance = TRUE)

predicted <- predict(model, newdata = test_df)
predicted_labels <- ifelse(predicted > 0.5, 1, 0)
randomForest_accuracy <- sum(predicted_labels == test_df$label) / nrow(test_df) * 100

# Calculate precision
randomForest_precision <- sum(predicted_labels[test_df$label == 1] == 1) / sum(predicted_labels == 1) * 100

# Calculate recall
randomForest_recall <- sum(predicted_labels[test_df$label == 1] == 1) / sum(test_df$label == 1) * 100

# Calculate F1 score
randomForest_f1_score <- 2 * (randomForest_precision * randomForest_recall) / (randomForest_precision + randomForest_recall)

