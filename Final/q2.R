#Open Feature
is_without_2015 = FALSE
is_final_optimization = TRUE


library(Matrix)
library(ISLR)
library(ROCR)
data = read.csv('D:\\Users\\lenovo\\Desktop\\Courses\\ECE625\\Final_1202\\dataset_1202.CSV',header = TRUE)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
if(is_without_2015)
{
  trainData = data[train,c(-1,-13,-14,-15,-29,-23)] # - taxroll_number, X2015, X2016, increase_average, lon, lat,l5,multi
  testData = data[test,c(-1,-13,-14,-15,-29,-23)]
  ytrain = trainData$trend # 19 -- trend
  ytest = testData$trend # 19 -- trend
  
  LineartrainData = trainData[,c(-12,-13)]# use this to filter parameters after Finding best subset
  LineartestData = testData[,c(-12,-13)]
}else
{
  trainData = data[train,c(-1,-14,-15,-29,-23)] # - taxroll_number,X2016, increase_average, lon, lat,l5,multi
  testData = data[test,c(-1,-14,-15,-29,-23)]
  ytrain = trainData$trend # 19 -- trend
  ytest = testData$trend # 19 -- trend
  
  LineartrainData = trainData[,c(-13,-14)]# use this to filter parameters after Finding best subset
  LineartestData = testData[,c(-13,-14)]
  
}


#logistic regression
library(boot)
glm.fit=glm(trend~.,data=LineartrainData,family="binomial")
summary(glm.fit)
coef(glm.fit)
glm.pro=predict(glm.fit,LineartestData,type = "response")
glm.pre=ifelse(glm.pro>0.5,1,0)
cm.glm=table(LineartestData$trend,glm.pre)
cm.glm
glm.error=mean(glm.pre!=LineartestData$trend)
glm.error


#LDA
library(MASS)
lda.fit=lda(trend~.,LineartrainData,scale = TRUE)
lda.pre=predict(lda.fit,testData)
cm.lda=table(truth = testData$trend,predict = lda.pre$class)
cm.lda
lda.error=mean(lda.pre$class!=testData$trend)
lda.error


#QDA  # TODO: There might be some problems here, because QDA should not be this high
qda.fit=qda(trend~.,data = LineartrainData,scale = TRUE)

qda.pre=predict(qda.fit,testData)
cm.qda=table(testData$trend,qda.pre$class)
cm.qda
qda.error=mean(qda.pre$class!=testData$trend)
qda.error


#liquidSVM
library(liquidSVM)
trainData$trend = as.factor(trainData$trend)
testData$trend = as.factor(testData$trend)
liquidfit = svm(trend~., trainData,min_gamma=4e-5)
result = test(liquidfit,testData)
errors(result)
table(predict=result, truth=testData$trend)




#neural Network
#Data scaling
maxs <- apply(trainData, 2, max) 
mins <- apply(trainData, 2, min)
scaledTrain <- as.data.frame(scale(trainData, center = mins, scale = maxs - mins))
maxs <- apply(testData, 2, max) 
mins <- apply(testData, 2, min)
scaledTest <- as.data.frame(scale(testData, center = mins, scale = maxs - mins))

library(neuralnet)
n <- names(scaledTrain)
f <- as.formula(paste("trend ~", paste(n[!n %in% "trend"], collapse = " + ")))
nn <- neuralnet(f,data=scaledTrain,hidden=6,act.fct = "logistic",linear.output = FALSE, err.fct = "ce",lifesign = 'full',threshold = 0.1,
                stepmax = 1e+06)
pr.nn <- compute(nn,scaledTest[,-18])
pr.nn_ <- pr.nn$net.result
nn.pred = as.numeric(pr.nn_ > 0.5)
cm.nn=table(scaledTest$trend,nn.pred)
cm.nn
nn.error=mean(nn.pred!=scaledTest$trend)
nn.error

# Boosting
library(gbm)
set.seed(1)
trainData$trend = as.numeric(trainData$trend-1)
boost.trend = gbm(trend~.,data=trainData, distribution = "bernoulli", n.trees = 1000, interaction.depth = 4 )
summary(boost.trend)
yhat.pre = predict(boost.trend, newdata=testData,n.trees=1000,type="response")
yhat.trend=ifelse(yhat.pre>=0.5,1,0)
table(pred = yhat.trend,truth = ytest)
mean(yhat.trend != ytest)


#random forest
if(is_final_optimization)
{
  data = read.csv('D:\\Users\\lenovo\\Desktop\\Courses\\ECE625\\Final_1202\\dataset_1202_with_pre.CSV',header = TRUE)
}
Trend=ifelse(data$trend==0,"Decrease","Increase")
data1=data.frame(data,Trend)
set.seed(1)
test1=sample(nrow(data1),nrow(data1)/10)
train1=-test1
if(is_without_2015)
{
  trainData1=data1[train1,c(-1,-13,-14,-15,-24)]
  testData1=data1[test1,c(-1,-13,-14,-15,-24)]
  
}else
{
  trainData1=data1[train1,c(-1,-14,-15,-24)]
  testData1=data1[test1,c(-1,-14,-15,-24)]
}
ytrain1=trainData1$Trend
ytest1=testData1$Trend

library(randomForest)
set.seed(1)
rf.trend = randomForest(Trend~.,data=trainData1,mtry=5,importance=TRUE)
yhat.rf = predict(rf.trend,newdata=testData1)
table(yhat.rf,ytest1)
importance(rf.trend)
mean(yhat.rf == ytest1)




