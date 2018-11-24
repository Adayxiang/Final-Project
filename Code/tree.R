# tree classification
library(Matrix)
library(ISLR)
data = read.csv('E:/WDY classes/ECE625/Final-Project-master 3/Dataset/dataset_1118 2.csv',header = TRUE)
attach(data)
Trend=ifelse(trend==0,"Decrease","Increase")
data=data.frame(data,Trend)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
trainData = data[train,c(-1,-14,-15,-16,-17,-24)] # - taxroll_number, X2016, increase_average, lon, lat, trend
testData = data[test,c(-1,-14,-15,-16,-17,-24)]
ytrain = trainData[,24] # 24 -- Trend
ytest = testData[,24] # 24 -- Trend

# classification tree
#library(tree)
#tree.trend=tree(Trend~.,trainData)
#summary(tree.trend)
#plot(tree.trend)
#text(tree.trend,pretty=0)
#tree.pred=predict(tree.trend,testData,type="class")
#table(tree.pred,ytest)
#set.seed(3)
#cv.trend=cv.tree(tree.trend,FUN=prune.misclass)
#names(cv.trend)
#cv.trend
#par(mfrow=c(1,2))
#plot(cv.trend$size,cv.trend$dev,type="b")
#plot(cv.trend$k,cv.trend$dev,type="b")

# random Forest
library(randomForest)
set.seed(1)
#ForestTrainData = trainData
#ForestTrainData[,'trend'] = as.factor(ForestTrainData[,'trend'])
rf.trend = randomForest(Trend~.,data=trainData,mtry=5,importance=TRUE)
yhat.rf = predict(rf.trend,newdata=testData)
table(yhat.rf,ytest)
importance(rf.trend)
varImpPlot(rf.trend)
mean(yhat.rf == ytest)

# Boosting
library(Matrix)
library(ISLR)
data = read.csv('E:/WDY classes/ECE625/Final-Project-master 3/Dataset/dataset_1124.csv',header = TRUE)
attach(data)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
trainData = data[train,c(-1,-13,-14,-15,-16,-17)] # - taxroll_number, X2016, increase_average, lon, lat, trend
testData = data[test,c(-1,-13,-14,-15,-16,-17)]
ytrain = trainData[,18] # 24 -- Trend
ytest = testData[,18] # 24 -- Trend

library(gbm)
set.seed(1)
boost.trend = gbm(trend~.,data=trainData, distribution = "bernoulli", n.trees = 1000, interaction.depth = 4)
yhat.trend = predict(boost.trend, newdata=testData,n.trees=1000,type="response")
yhat.trend=ifelse(yhat.trend>=0.5,1,0)
table(yhat.trend,ytest)
mean(yhat.trend == ytest)
