library(gbm)
set.seed(1)
# gaussian - regression
# bernoulli - classification
# interaction.depth - limit of each tree depth
boost.boston =gbm(medv~.,data=Boston [train ,], distribution="gaussian",n.trees =5000 , interaction.depth =4)
yhat.boost=predict (boost.boston ,newdata =Boston [-train ,],n.trees =5000)
mean(( yhat.boost -boston.test)^2)

# change lambda
boost.boston =gbm(medv~.,data=Boston [train ,], distribution="gaussian", n.trees =5000, interaction.depth =4, shrinkage =0.2,verbose =F)
yhat.boost=predict(boost.boston, newdata=Boston [-train ,],n.trees =5000)
mean(( yhat.boost -boston.test)^2)