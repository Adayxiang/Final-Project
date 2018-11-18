library(Matrix)
library(ISLR)
data = read.csv('D:\\Users\\lenovo\\Desktop\\Courses\\ECE625\\project\\dataset_delated.CSV',header = TRUE)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
trainData = data[train,c(-13,-6,-14,-16)]
testData = data[test,c(-13,-6,-14,-16)]
ytrain = trainData[,13]
ytest = testData[,13]

# best subset
library(leaps)
regfit.best = regsubsets(trend~.,data=trainData,nvmax = 12)
test.mat = model.matrix(trend~.,data=testData)
val.errors = rep(NA,12)
for(i in 1:12){
  coefi=coef(regfit.best,id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean(((ytest-pred)^2)^(1/2))
}
val.errors
which.min(val.errors)
coef(regfit.best,which.min(val.errors))

#logistic regression
library(boot)
glm.fit=glm(trend~has_garage+has_fireplace+walkout_basement+air_conditioning+X2015,data=trainData,family="binomial")
coef(glm.fit)
glm.pro=predict(glm.fit,testData)
glm.pre=ifelse(glm.pro>0.5,1,0)
cm.glm=table(testData$trend,glm.pre)
cm.glm
glm.error=mean(glm.pre!=testData$trend)
glm.error


#LDA
library(MASS)
lda.fit=lda(trend~has_garage+has_fireplace+walkout_basement+air_conditioning+X2015,trainData)
lda.pre=predict(lda.fit,testData)
cm.lda=table(truth = testData$trend,predict = lda.pre$class)
cm.lda
lda.error=mean(lda.pre$class!=testData$trend)
lda.error


#QDA
qda.fit=qda(trend~has_garage+has_fireplace+walkout_basement+air_conditioning+X2015,trainData)
qda.pre=predict(qda.fit,testData)
cm.qda=table(testData$trend,qda.pre$class)
cm.qda
qda.error=mean(qda.pre$class!=testData$trend)
qda.error

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

#liquidSVM
library(liquidSVM)
#data1 = read.csv('~/documents/study/ECE625/project/Final-Project-master/Dataset/datasetLiquidSVM.csv',header = TRUE)
#set.seed(1)
#test = sample(nrow(data1),nrow(data1)/10)
#train = -test
#trainData1 = data1[train,]
#testData1 = data1[test,]
trainData[,13] = as.factor(trainData[,13])
testData[,13] = as.factor(testData[,13])
liquidfit = svm(trend~., trainData,min_gamma=0.04)
result = test(liquidfit,testData)
errors(result)
table(predict=result, truth=testData[,13])



