library(caTools)
library(funModeling) 
library(tidyverse) 
library(ROCR)
library(Hmisc)
library(caret)
library("ggpubr")
library('fastDummies')

dataset = read.csv('D:/Parvathi_Sanjana_IIP/dataset_sdn.csv')

dataset <- na.omit(dataset)
dataset <- dataset[,c('switch','src','dst','Protocol','port_no','label')]
dataset <- dataset[1:10000,]
dataset


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


Model <- lm(label ~.,data=training_data)
summary(Model)


prob <- predict(Model, testing_data)
pred <- ifelse(prob>0.5, 1, 0)

plot(testing_data$label,type = "l",lty=1.8,col="red")
lines(pred, type = "l",lty = 1.8,col="blue")

rmse <- sqrt((mean(pred-testing_data$label)^2)/10000)
rmse

missing_classerr <- mean(pred != testing_data$label)
missing_classerr
print(paste('Accuracy =', 1 - missing_classerr))


