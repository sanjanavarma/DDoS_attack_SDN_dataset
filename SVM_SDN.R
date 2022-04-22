library(funModeling) 
library(tidyverse) 
library(ROCR)
library(Hmisc)
library("ggpubr")
library('fastDummies')
library(caTools)
library(e1071)

dataset = read.csv('D:/Parvathi_Sanjana_IIP/dataset_sdn.csv')
dataset$pktcount <- scale(dataset$pktcount)

dataset <- na.omit(dataset)
dataset <- dataset[,c('switch','src','dst','Protocol','pktcount','port_no','label')]
dataset <- dataset[20000:70000,]
dataset
table(dataset$switch)
table(dataset$src)
table(dataset$dst)
table(dataset$Protocol)
table(dataset$port_no)
table(dataset$label)
library(caret)


dataf <- dummy_cols(dataset, select_columns = c('src', 'dst', 'Protocol','switch','port_no'))
dataf <- dataf
dataf


drop <- c("src","dst","Protocol","port_no","switch")
df = dataf[,!(names(dataf) %in% drop)]
df


set.seed(123)
split = sample.split(df$label, SplitRatio = 0.8)
split
training_data = subset(df, split == TRUE)
testing_data = subset(df, split == FALSE)


classifier = svm(formula = label ~ .,
                 data = training_data,
                 type = 'C-classification',
                 kernel = 'linear')
training_data
y_pred = predict(classifier, newdata = testing_data)
y_pred
cm = table(testing_data$label, y_pred)
cm


missing_classerr <- mean(y_pred != testing_data$label)
missing_classerr
print(paste('Accuracy =', 1 - missing_classerr))
