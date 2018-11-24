library(Matrix)
library(ISLR)
data = read.csv('D:/Users/lenovo/Desktop/Courses/ECE625/project/20181123/dataset_1118.csv',header = TRUE)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
trainData = data[train,c(-1,-13,-14,-15,-16,-17)] # - taxroll_number, X2015, X2016, increase_average, lon, lat
testData = data[test,c(-1,-13,-14,-15,-16,-17)]
ytrain = trainData[,18] # 19 -- trend
ytest = testData[,18] # 19 -- trend

LineartrainData = trainData[c(-6, -10,-11,-17,-21,-23)]# use this to filter parameters after Finding best subset
LineartestData = testData[c(-6, -10,-11,-17,-21,-23)]


# best subset
library(leaps)
regfit.best = regsubsets(trend~.,data=trainData,nvmax = 16)
test.mat = model.matrix(trend~.,data=testData)
val.errors = rep(NA,16)
for(i in 1:16){
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


#LDA
library(MASS)
lda.fit=lda(trend~.,LineartrainData)
lda.pre=predict(lda.fit,testData)
cm.lda=table(truth = testData$trend,predict = lda.pre$class)
cm.lda
lda.error=mean(lda.pre$class!=testData$trend)
lda.error


#QDA  # TODO: There might be some problems here, because QDA should not be this high
qda.fit=qda(trend~.,LineartrainData)

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
trainData[,18] = as.factor(trainData[,18])
testData[,18] = as.factor(testData[,18])


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
table(predict=result, truth=testData[,18])
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