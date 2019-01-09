library(ISLR)
library(gbm)
set.seed(103)

# a
Hitters = na.omit(Hitters)
Hitters$Salary = log(Hitters$Salary)

# b
train = 1:200
X.train = Hitters[train,]
X.test = Hitters[-train,]
Y.train = X.train$Salary
Y.test = X.test$Salary

# c
pows = seq(-10, -0.2, by=.1)
lambdas = 10^pows
N = length(lambdas)
train.err = rep(NA, N)
test.err = rep(NA, N)
for (i in 1:N){
	boost = gbm(Salary~.,data=X.train, distribution="gaussian",n.trees =1000 , shrinkage=lambdas[i])
	yhat.boost = predict(boost, newdata = X.train, n.trees=1000)
	train.err[i] = mean(( yhat.boost - Y.train)^2)
	
	yhat.boost = predict(boost, newdata = X.test, n.trees=1000)
	test.err[i] = mean(( yhat.boost - Y.test)^2)
}

plot(lambdas, train.err, type="b", col="red")

# d
plot(lambdas, test.err, type="b", col="blue")


# e
lm.fit = lm(Salary ~ ., data = X.train)
lm.pred = predict(lm.fit, X.test)
mean((Y.test - lm.pred)^2)

# f
boost.best = gbm(Salary ~ ., data = X.train, distribution = "gaussian", 
    n.trees = 1000, shrinkage = lambdas[which.min(test.err)])
summary(boost.best)

# g
library(randomForest)

bag = randomForest(Salary~.,data=X.train, mtry=ncol(X.train)-1, importance = TRUE)
yhat.bag = predict (bag, newdata=X.test)
mean((Y.test - yhat.bag)^2)