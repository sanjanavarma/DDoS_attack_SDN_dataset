# Loading package
library(ClusterR)
library(cluster)
library(VIM)
library("factoextra")
library('fastDummies')
library(Hmisc)
dataset = read.csv('D:/Parvathi_Sanjana_IIP/dataset_sdn.csv')
describe(dataset)
dataset <- dataset[20000:70000,]
dataset <- na.omit(dataset)
aggr(dataset)
dataset_with_label <- dataset[,c('switch','src','dst','Protocol','port_no','label')]
dataset <- dataset[,c('switch','src','dst','pktcount','Protocol','port_no')]
dataset$pktcount <- scale(dataset$pktcount)
dataset

dataf_1 <- dummy_cols(dataset, select_columns = c('src', 'dst', 'Protocol','switch','port_no'))
dataf <- dataf_1
dataf


drop <- c("src","dst","Protocol","port_no","switch")
df = dataf[,!(names(dataf) %in% drop)]
df


# Fitting K-Means clustering Model 
# to training dataset
set.seed(240) # Setting seed
kmeans.re <- kmeans(df, centers = 2, nstart = 20)
kmeans.re


kmeans.re$cluster
kmeans.re$centers
p1 <- fviz_cluster(kmeans.re, geom = "point", data = df) + ggtitle("k = 2")
p1
