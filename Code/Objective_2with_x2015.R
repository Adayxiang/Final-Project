library(Matrix)
library(ISLR)
data = read.csv('~/documents/study/ECE625/project/Final-Project-master 3/Dataset/dataset_1118.csv',header = TRUE)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
trainData = data[train,c(-1,-14,-15,-16,-17)] # - taxroll_number, X2016, increase_average, lon, lat
testData = data[test,c(-1,-14,-15,-16,-17)]
ytrain = trainData[,19] # 19 -- trend
ytest = testData[,19] # 19 -- trend

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
glm.fit=glm(trend~has_garage+has_fireplace+ building_count+walkout_basement+x_2015+air_conditioning+RESIDENTIAL+INDUSTRIAL+MULTI.RES,data=trainData,family="binomial")
coef(glm.fit)
glm.pro=predict(glm.fit,testData)
glm.pre=ifelse(glm.pro>0.5,1,0)
cm.glm=table(testData$trend,glm.pre)
cm.glm
glm.error=mean(glm.pre!=testData$trend)
glm.error


#LDA
library(MASS)
lda.fit=lda(trend~has_garage+has_fireplace+ building_count+walkout_basement+x_2015+air_conditioning+RESIDENTIAL+INDUSTRIAL+MULTI.RES,trainData)
lda.pre=predict(lda.fit,testData)
cm.lda=table(truth = testData$trend,predict = lda.pre$class)
cm.lda
lda.error=mean(lda.pre$class!=testData$trend)
lda.error


#QDA
qda.fit=qda(trend~has_garage+has_fireplace+ building_count+walkout_basement+x_2015+air_conditioning+RESIDENTIAL+INDUSTRIAL+MULTI.RES,trainData)
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
trainData[,19] = as.factor(trainData[,19])
testData[,19] = as.factor(testData[,19])
liquidfit = svm(trend~., trainData,min_gamma=0.04)
result = test(liquidfit,testData)
errors(result)
table(predict=result, truth=testData[,19])



