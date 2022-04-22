library(funModeling) 
library(tidyverse) 
library(ROCR)
library(Hmisc)
library("ggpubr")
library('fastDummies')
library(caTools)

dataset = read.csv('D:/Parvathi_Sanjana_IIP/dataset_sdn.csv')

dataset$pktcount <- scale(dataset$pktcount)

dataset <- na.omit(dataset)
dataset <- dataset[,c('switch','src','dst','pktcount','Protocol','port_no','label')]
dataset <- dataset[20000:70000,]

barplot(table(dataset$switch),xlab="Switch number",ylab="Frequency")
barplot(table(dataset$Protocol),xlab="Protocol",ylab="Frequency",col = c("#eb8060", "#b9e38d", "#a1e9f0"))
barplot(table(dataset$port_no),xlab="Port Number",ylab="Frequency",col = c("#eb8060", "#b9e38d", "#a1e9f0", "#d9b1f0"))
barplot(table(dataset$label),xlab="Label",ylab="Frequency",col = c("#b9e38d","#eb8060"))
library(caret)


dataf <- dummy_cols(dataset, select_columns = c('src', 'dst','Protocol','switch','port_no'))
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

library(neuralnet)

#n <- neuralnet(label~.,
 #              data = training_data, linear.output = F, hidden=8,act.fct="logistic", threshold=0.01)

n <- neuralnet(label~.,
               data = training_data, linear.output = F,algorithm = "rprop+",
               hidden=c(10,3),
               threshold=0.1,
               stepmax = 1e+06)

plot(n)

predict_reg <- predict(n, 
                       testing_data, type = "response")

Predict=compute(n,testing_data)
Predict

prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)

pred
missing_classerr <- mean(pred != testing_data$label)
missing_classerr
print(paste('Accuracy =', 1 - missing_classerr))
