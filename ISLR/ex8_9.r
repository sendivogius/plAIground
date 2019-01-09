library(ISLR)
library(tree)
set.seed(1013)

# a
num_train = 800
train = sample(1:nrow(OJ), num_train)
OJ.train = OJ[train,]
OJ.test = OJ[-train,]

# b
tree.oj= tree(Purchase~., OJ.train)
summary(tree.oj)

# c
tree.oj

# d
plot(tree.oj)
text(tree.oj, pretty=0)

# e
predicted = predict(tree.oj, OJ.test, type = "class")
table(OJ.test$Purchase, predicted)
mean(OJ.test$Purchase == predicted)

# f
cv.oj= cv.tree(tree.oj)

# g
plot(cv.oj$size ,cv.oj$dev ,type="b")

# h
best_size = cv.oj$size[which.min(cv.oj$dev)]

# i
prune.oj = prune.tree(tree.oj ,best =best_size)
plot(prune.oj)
text(prune.oj ,pretty = 0)
predicted = predict(prune.oj, OJ.test, type = "class")
table(OJ.test$Purchase, predicted)
mean(OJ.test$Purchase == predicted)


