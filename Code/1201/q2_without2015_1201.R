library(Matrix)
library(ISLR)
data = read.csv('D:\\Users\\lenovo\\Desktop\\Courses\\ECE625\\project\\dataset_1124.CSV',header = TRUE)
#data$trend = as.factor(data$trend)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
trainData = data[train,c(-1,-13,-14,-15,-16,-17,-29,-23)] # - taxroll_number, X2015, X2016, increase_average, lon, lat,l5,multi
testData = data[test,c(-1,-13,-14,-15,-16,-17,-29,-23)]
ytrain = trainData$trend # 19 -- trend
ytest = testData$trend # 19 -- trend

LineartrainData = trainData[,c(-4,-6,-11,-13,-15,-20)]# use this to filter parameters after Finding best subset
LineartestData = testData[,c(-4,-6,-11,-13,-15,-20)]

"
# best subset
library(leaps)
regfit.best = regsubsets(trend~.,data=trainData,nvmax = 16)
test.mat = model.matrix(trend~.,data=testData)
val.errors = rep(NA,16)
for(i in 1:16){
  coefi=coef(regfit.best,id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean(ytest*log(pred))
}
val.errors
which.min(val.errors)
coef(regfit.best,which.min(val.errors))
"

#logistic regression
library(boot)
glm.fit=glm(trend~.,data=LineartrainData,family="binomial")
summary(glm.fit)
coef(glm.fit)
glm.pro=predict(glm.fit,LineartestData)
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

"
#Using Loess Function
library(ISLR)
data = read.csv('~/documents/study/ECE625/project/Final-Project-master/Dataset/dataset_delated.csv',header = TRUE)
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
"

#liquidSVM
library(liquidSVM)
#data1 = read.csv('~/documents/study/ECE625/project/Final-Project-master/Dataset/datasetLiquidSVM.csv',header = TRUE)
#set.seed(1)
#test = sample(nrow(data1),nrow(data1)/10)
#train = -test
#trainData1 = data1[train,]
#testData1 = data1[test,]
trainData$trend = as.factor(trainData$trend)
testData$trend = as.factor(testData$trend)


#for Leave one out validation
"
set.seed(1)
gamma_vector = c(0.004,0.04,0.4,4)
ErrorsVec = c()
for (i in 1:4)
{
  liquidfit = svm(trend~., trainData,min_gamma=gamma_vector[i])
  result = test(liquidfit,testData)
  ErrorsVec[i] = errors(result)
}
ErrorsVec
"  

#"For single one
liquidfit = svm(trend~., trainData,min_gamma=4e-5)
result = test(liquidfit,testData)
errors(result)
table(predict=result, truth=testData$trend)
#"

#neural Network
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

plot(nn)


#Predicting medv using the neural network
pr.nn <- compute(nn,scaledTest[,-18])
pr.nn_ <- pr.nn$net.result
head(pr.nn_)
nn.pred = as.numeric(pr.nn_ > 0.5)

cm.nn=table(scaledTest$trend,nn.pred)
cm.nn
nn.error=mean(nn.pred!=scaledTest$trend)
nn.error

#Using another library to implement Mini-Batch GD
#install.packages("ANN2")
library(ANN2)
NN <- neuralnetwork(scaledTrain[,-14], scaledTrain$trend, hiddenLayers = c(5, 5),
                    learnRate = 0.0001, verbose = TRUE,lossFunction = "log", regression = FALSE,
                    standardize = TRUE, batchSize = 32, validLoss = TRUE,maxEpochs = 10000,
                    earlyStop = TRUE, earlyStopEpochs = 1000, earlyStopTol = 0.01)
plot(NN)
pred <- predict(NN, newdata = scaledTest[,-14])
nn.pred = pred$predictions

cm.nn=table(scaledTest$trend,nn.pred)
cm.nn
nn.error=mean(nn.pred!=scaledTest$trend)
nn.error

"
pr.nn_ <- pr.nn$net.result*(max(scaledTrain$trend)-min(scaledTrain$trend))+min(scaledTrain$trend)# ( prediction * scale ) + center(medv)
test.r <- (scaledTest$trend)*(max(scaledTest$trend)-min(scaledTest$trend))+min(scaledTest$trend)# the same, restore scaled test data
MSE.nn <- sum((test.r - pr.nn_)^2)/nrow(scaledTest)
MSE.nn
"

#we then compare the two MSEs


# Boosting
library(gbm)
set.seed(1)
boost.trend = gbm(trend~.,data=trainData, distribution = "bernoulli", n.trees = 1000, interaction.depth = 4 )
summary(boost.trend)
yhat.pre = predict(boost.trend, newdata=testData,n.trees=1000,type="response")
yhat.trend=ifelse(yhat.pre>=0.5,1,0)
table(pred = yhat.trend,truth = ytest)
mean(yhat.trend != ytest)



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

#neural network
library(neuralnet)
maxs <- apply(trainData, 2, max) 
mins <- apply(trainData, 2, min)
scaledTrain <- as.data.frame(scale(trainData, center = mins, scale = maxs - mins))
f <- as.formula(paste("trend ~", paste(n[!n %in% "trend"], collapse = " + ")))
f
nn <- neuralnet(f,data=scaledTrain,hidden=c(5),linear.output = FALSE,lifesign = 'full',threshold = 0.5, err.fct = 'ce')

scaledTest <- as.data.frame(scale(testData, center = mins, scale = maxs - mins))

pr.nn <- compute(nn,scaledTest)
pr.nnRaw <- pr.nn$net.result
pr.nnPred = ifelse(pr.nnRaw>=0.5,1,0)
table(pred = pr.nnPred, truth = testData$trend)
mean(pr.nnPred!=testData$trend)



title("With 2015")
legend("bottomright",legend=c("Logistic","LDA","QDA","SVM","Boostig"),col=c("red","black","blue","green","pink"),lty=1,lwd=2,cex=.8)

