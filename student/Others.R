#bagging
library(randomForest)
d1.bagging=randomForest(pass_fail~.-id-G3-pass_fail-grades,data = d1.train,mtry=10,importance=TRUE)
d1.bagging

#randomForest
d1.RandomForest=randomForest(pass_fail~.-id-G3-pass_fail-grades,data = d1.train,mtry=6,importance=TRUE)
d1.RandomForest
importance(d1.RandomForest)
varImpPlot(d1.RandomForest)

#Boosting
library(gbm)
d1.train$pass_fail = ifelse(d1$G3>=10,0,1)
d1.test$pass_fail = ifelse(d1$G3>=10,0,1)
d1.boosting=gbm(pass_fail~.-id-G3-pass_fail-grades,data = d1.train,distribution="bernoulli",n.trees=5000,interaction.depth=4)
summary(d1.boosting)
boost.probs=predict(d1.boosting,newdata=d1.test,type="response",n.trees=5000)
boost.pred=rep(0,200)
boost.pred[boost.probs>=0.5]=1
boost.pred
table(boost.pred,d1.test$pass_fail)