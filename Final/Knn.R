
#using knn to predict
library(kknn)
locationTrain = trainData[,c("lon","lat","trend")]
locationTest = data[,c("lon","lat","trend")]
golf.kknn <- kknn(trend~.,train = locationTrain,test = locationTest,k=9,scale=T,distance=2,kernel= "rectangular")
PredLoc = ifelse(apply(golf.kknn$CL,1,mean) >0.5,1,0)
plot(location[golf.kknn$C[1,1],1:2], xlim = c(-113.8, -113.2), ylim = c(53.3, 53.7), col = "black", cex = 2, pch = 20)
points(location[golf.kknn$C[1,2],1:2],col = "red",cex = 2, pch = 20)
points(location[golf.kknn$C[1,3],1:2],col = "blue",cex = 2, pch = 20)
points(location[golf.kknn$C[1,4],1:2],col = "red",cex = 2, pch = 20)
points(location[golf.kknn$C[1,5],1:2],col = "red",cex = 2, pch = 20)
points(location[golf.kknn$C[1,6],1:2],col = "blue",cex = 2, pch = 20)
points(location[golf.kknn$C[1,7],1:2],col = "red",cex = 2, pch = 20)
points(location[golf.kknn$C[1,8],1:2],col = "blue",cex = 2, pch = 20)
points(location[golf.kknn$C[1,9],1:2],col = "red",cex = 2, pch = 20)