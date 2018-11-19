library(ISLR)
data = read.csv('D:/Study/ECE 625/final project/dataset_1118.csv',header = TRUE)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
trainData = data[train,c(-1,-14,-15,-16,-17,-24,-29)]
testData = data[test,c(-1,-14,-15,-16,-17,-24,-29)]
ytrain = trainData[,12]
ytest = testData[,12]

#linear regression
lm.fit=lm(x_2015~.,data = trainData)
summary(lm.fit)
#calculate train error
(mean((predict(lm.fit,newdata = trainData) -  trainData$x_2015)^2)^(1/2))
plot(predict(lm.fit,newdata = trainData)[seq(1,100)])
points(ytrain[seq(1,100)],col =   2)
plot(predict(lm.fit,newdata = trainData))
points(ytrain,col = 2)
#calculate test error
(mean((predict(lm.fit,newdata = testData) -  ytest)^2)^(1/2))


# best subset
library(leaps)
regfit.best = regsubsets(x_2015~.,data=trainData,nvmax = 20)
test.mat = model.matrix(x_2015~.,data=testData)
val.errors = rep(NA,20)
for(i in 1:20){
  coefi=coef(regfit.best,id=i)
  pred = test.mat[,names(coefi)]%*%coefi
  val.errors[i] = mean(((ytest-pred)^2)^(1/2))
}
val.errors
which.min(val.errors)
coef(regfit.best,which.min(val.errors))


# ridge regression
x = model.matrix(trainData$x_2015~.,trainData)
y = data[train,]$x_2015
xtestR = model.matrix(testData$x_2015~.,testData)
yR = data[test,]$x_2015

y.test=y[test]
library(Matrix)
library(foreach)
library(glmnet)
grid = 10 ^ seq(10,-2,length=100)
ridge.mod = glmnet(x,y,alpha=0,lambda=grid)
dim(coef(ridge.mod))
cv.out=cv.glmnet(x,y,alpha=0) 
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam
ridge.pred=predict(ridge.mod,s=bestlam,newx=xtestR) 
mean(((ridge.pred-yR)^2)^(1/2)) 


#Using cubic spines
library(splines)
fit=lm(x_2015~
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
       ,data=trainData)

#calculate train error
pred=predict(fit,newdata=trainData,se = T)#it might be some problems of this predict, will check after meeting
predValue = pred$fit
(mean((pred$fit-ytrain)^2))^0.5

#calculate test error
pred=predict(fit,newdata=testData,se = T)
predValue = pred$fit
(mean((pred$fit-ytest)^2))^0.5

