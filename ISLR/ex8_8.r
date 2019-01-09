set.seed(1)
train=sample(1:nrow(Carseats), nrow(Carseats)/2)
Carseats.train = Carseats[train,]
Carseats.test = Carseats[-train,]
tree.carseats = tree(Sales~., Carseats.train)

pred = predict(tree.carseats, Carseats.test)
mean((pred-Carseats.test$Sales)^2)
plot(tree.carseats )
text(tree.carseats ,pretty =0)

cv.carseats = cv.tree(tree.carseats)
plot(cv.carseats$size ,cv.carseats$dev ,type="b")

best_size = cv.carseats$size[which.min(cv.carseats$dev)]
prune.carseats =prune.tree(tree.carseats ,best =best_size)
plot(prune.carseats )
text(prune.carseats ,pretty =0)
pred = predict(prune.carseats, Carseats.test)
mean((pred-Carseats.test$Sales)^2)

set.seed(1)
bag.carseats = randomForest(Sales~.,data=Carseats.train, mtry=10, importance =TRUE)
yhat.bag = predict(bag.carseats ,newdata=Carseats.test)
mean(( yhat.bag -Carseats.test$Sales)^2)

set.seed(1)
bag.carseats = randomForest(Sales~.,data=Carseats.train, mtry=4, importance =TRUE)
yhat.bag = predict(bag.carseats ,newdata=Carseats.test)
mean(( yhat.bag -Carseats.test$Sales)^2)