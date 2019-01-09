library(ROCR)
rocplot = function(pred, truth, ...){
	predob = prediction(pred, truth)
	perf = performance(predob, "tpr", "fpr")
	plot(perf,...)
}

svmfit.opt = svm(y~., data=dat[train ,], kernel="radial",gamma =2, cost=1, decision.values =T)
fitted = attributes (predict (svmfit.opt ,dat[train ,], decision.values =TRUE))$decision.values
par(mfrow =c(1,2))
rocplot (fitted ,dat [train ,"y"], main= "Training Data")

svmfit.flex=svm(y~., data=dat[train ,], kernel ="radial", gamma =50, cost=1, decision.values =T)
fitted =attributes (predict (svmfit.flex ,dat[train ,], decision.values =T))$decision.values
rocplot (fitted ,dat [train ,"y"], add =T,col ="red ")

fitted =attributes (predict (svmfit.opt ,dat[-train ,], decision.values =T))$decision.values
rocplot (fitted ,dat [-train ,"y"], main ="Test Data")
fitted =attributes (predict (svmfit.flex ,dat[-train ,], decision.values =T))$decision.values
rocplot (fitted ,dat [-train ,"y"], add=T,col =" red ")


# Multi
set.seed (1)
x=rbind(x, matrix (rnorm (50*2) , ncol =2))
y=c(y, rep (0 ,50) )
x[y==0 ,2]= x[y==0 ,2]+2
dat=data.frame(x=x, y=as.factor (y))
par(mfrow =c(1,1))
plot(x,col =(y+1))

svmfit =svm(y~., data=dat , kernel ="radial", cost =10, gamma =1)
plot(svmfit , dat)

# gene
library (ISLR)
names(Khan)
dim( Khan$xtrain )
dim( Khan$xtest )
length (Khan$ytrain )
length (Khan$ytest )
table(Khan$ytrain )
table(Khan$ytest )

dat=data.frame(x=Khan$xtrain , y=as.factor ( Khan$ytrain ))
out=svm(y~., data=dat , kernel ="linear",cost =10)
summary (out)
table(out$fitted , dat$y)

dat.te=data.frame(x=Khan$xtest , y=as.factor (Khan$ytest ))
pred.te=predict (out, newdata =dat.te)
table(pred.te , dat.te$y)