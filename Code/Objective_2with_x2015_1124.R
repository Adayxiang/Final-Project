library(Matrix)
library(ISLR)
library(ROCR)
library(pROC)
data = read.csv('D:/Study/ECE 625/final project/dataset_1124.CSV',header = TRUE)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
trainData = data[train,c(-1,-14,-15,-16,-17)] # - taxroll_number, X2016, increase_average, lon, lat
testData = data[test,c(-1,-14,-15,-16,-17)]
ytrain = trainData[,19] # 19 -- trend
ytest = testData[,19] # 19 -- trend

LineartrainData = trainData[c(-6, -10,-11,-18,-22,-24)]# use this to filter parameters after Finding best subset
LineartestData = testData[c(-6, -10,-11,-18,-22,-24)]

# best subset
library(leaps)
regfit.best = regsubsets(trend~.,data=trainData,nvmax = 17)
test.mat = model.matrix(trend~.,data=testData)
val.errors = rep(NA,17)
for(i in 1:17){
  coefi=coef(regfit.best,id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean(((ytest-pred)^2)^(1/2))
}
val.errors
which.min(val.errors)
coef(regfit.best,which.min(val.errors))

#logistic regression
library(boot)
glm.fit=glm(trend~.,data=LineartrainData,family="binomial")
coef(glm.fit)
glm.pro=predict(glm.fit,LineartestData)
glm.pre=ifelse(glm.pro>0.5,1,0)
cm.glm=table(LineartestData$trend,glm.pre)
cm.glm
glm.error=mean(glm.pre!=LineartestData$trend)
glm.error
glm.pred=prediction(glm.pro,LineartestData$trend)
perf=performance(glm.pred,"tpr","fpr")
plot(perf,col="red",lwd=2)

#LDA
library(MASS)
lda.fit=lda(trend~.,LineartrainData)
lda.pre=predict(lda.fit,LineartestData)
cm.lda=table(truth = LineartestData$trend,predict = lda.pre$class)
cm.lda
lda.error=mean(lda.pre$class!=LineartestData$trend)
lda.error
lda.pred=prediction(lda.pre$posterior[,2],LineartestData$trend)
lda.perf=performance(lda.pred,"tpr","fpr")
plot(lda.perf,add=TRUE,col="black",lwd=2)


#QDA
qda.fit=qda(trend~.,LineartrainData)
qda.pre=predict(qda.fit,LineartestData)
cm.qda=table(LineartestData$trend,qda.pre$class)
cm.qda
qda.error=mean(qda.pre$class!=LineartestData$trend)
qda.error
qda.pred=prediction(qda.pre$posterior[,2],LineartestData$trend)
qda.perf=performance(qda.pred,"tpr","fpr")
plot(qda.perf,add=TRUE,col="blue",lwd=2)

#Using Loess Function
library(ISLR)
data = read.csv('D:/Study/ECE 625/final project/dataset_delated.csv',header = TRUE)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
trainData = data[train,c(-13,-6,-15)]
testData = data[test,c(-13,-6,-15)]
ytrain = data[train,16]
ytest = data[test,16]
library(splines)
fit=lm(X2016~
         +bs(effective_build_year,df=6)
       +bs(net_area,df = 6)
       +basement_finished
       +has_garage
       +has_fireplace
       +fully_taxable
       +bs(building_count,df = 6)
       +walkout_basement
       +air_conditioning
       +bs(tot_gross_area_description,df = 6)
       +bs(lot_size,df = 6)
       +bs(assessed_value, df = 6)
       +trend
       ,data=trainData)

pred=predict(fit,newdata=testData[,-13],se = T)
predValue = pred$fit
result = predValue - ytest 
result.pred=rep(0,length(ytest))
result.pred[result>0]= 1
table(predict = result.pred, truth = testData[,14])
result.pre=prediction(result.pred,testData$trend)
result.perf=performance(result.pre,"tpr","fpr")
plot(result.perf)

#for Leave one out validation




#liquidSVM
library(liquidSVM)
#data1 = read.csv('D:/Study/ECE 625/final project/dataset_1118.csv',header = TRUE)
#set.seed(1)
#test = sample(nrow(data1),nrow(data1)/10)
#train = -test
#trainData1 = data1[train,]
#testData1 = data1[test,]
trainData[,19] = as.factor(trainData[,19])
testData[,19] = as.factor(testData[,19])

set.seed(1)
liquidfit = svm(trend~., trainData,min_gamma=1e-6,max_gamma=)
result = test(liquidfit,testData)
errors(result)
table(predict=result, truth=testData[,19])
plotROC(result,testData$trend)
model=rocSVM(trend~.,trainData,min_gamma=4e-6)
svm.result=test(model,testData)
plotROC(svm.result,testData$trend,type="l",add=TRUE,col="orange",lwd=2)
errors(svm.result)
train.error=liquidfit$train_errors$train_error
train.error
liquidfit$train_errors[which.min(train.error),]

# Boosting
library(gbm)
set.seed(1)
trainData = data[train,c(-1,-14,-15,-16,-17)] # - taxroll_number, X2016, increase_average, lon, lat
testData = data[test,c(-1,-14,-15,-16,-17)]
boost.trend = gbm(trend~.,data=trainData, distribution = "bernoulli", n.trees = 1000, interaction.depth = 4 )
yhat.pre = predict(boost.trend, newdata=testData,n.trees=1000,type="response")
yhat.trend=ifelse(yhat.pre>=0.5,1,0)
table(yhat.trend,ytest)
mean(yhat.trend == ytest)
boost.pred=prediction(yhat.pre,testData$trend)
boost.perf=performance(boost.pred,"tpr","fpr")
plot(boost.perf,add=TRUE,col="pink",lwd=2)

#random forest
Trend=ifelse(data$trend==0,"Decrease","Increase")
data1=data.frame(data,Trend)
set.seed(1)
test1=sample(nrow(data1),nrow(data1)/10)
train1=-test1
trainData1=data1[train1,c(-1,-14,-15,-16,-17,-24)]
testData1=data1[test1,c(-1,-14,-15,-16,-17,-24)]
ytrain1=trainData1[,24]
ytest1=testData1[,24]

library(randomForest)
set.seed(1)
rf.trend = randomForest(Trend~.,data=trainData1,mtry=5,importance=TRUE)
yhat.rf = predict(rf.trend,newdata=testData1)
table(yhat.rf,ytest1)
importance(rf.trend)
varImpPlot(rf.trend)
rf.pre=predict(rf.trend,newdata=testData1,type="prob")
rf.pred=prediction(rf.pre[,2],ytest1)
rf.perf=performance(rf.pred,"tpr","fpr")
plot(rf.perf,add=TRUE,col="purple",lwd=2)


title("With 2015")
legend("bottomright",legend=c("Logistic","LDA","QDA","SVM","Boostig"),col=c("red","black","blue","green","pink"),lty=1,lwd=2,cex=.8)
