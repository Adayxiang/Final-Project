library(ISLR)
data = read.csv('D:\\Users\\lenovo\\Desktop\\Courses\\ECE625\\project\\AfterEdit.CSV',header = TRUE)
set.seed(1)
test = sample(nrow(data),nrow(data)/10)
train = -test
trainData = data[train,c(-13,-6)]
testData = data[test,c(-13,-6)]
ytrain = trainData[,12]
ytest = testData[,12]

#linear regression
lm.fit=lm(assessed_value~.,data = trainData)
summary(lm.fit)
#calculate train error
mean((predict(lm.fit,newdata = trainData) -  ytrain)^2)
plot(predict(lm.fit,newdata = trainData)[seq(1,100)])
points(ytrain[seq(1,100)],col = 2)
plot(predict(lm.fit,newdata = trainData))
points(ytrain,col = 2)
#calculate test error
mean((predict(lm.fit,newdata = testData) -  ytest)^2)
plot(predict(lm.fit,newdata = testData)[seq(1,100)])
points(ytest[seq(1,100)],col = 2)
plot(predict(lm.fit,newdata = testData))
points(ytest,col = 2)

#Using Loess Function
library(splines)
fit=lm(assessed_value~
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

pred=predict(fit,newdata=testData,se = T)
predValue = pred$fit
mean((pred$fit-ytest)^2)
