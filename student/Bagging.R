nfolds = 5
foldPosition = sample(rep(1:nfolds, length.out = dim(d1)[1]))

Accuracy = replicate(nfolds,0)
Recall.No = replicate(nfolds,0)
Recall.Yes = replicate(nfolds,0)
Precision.No = replicate(nfolds,0)
Precision.Yes = replicate(nfolds,0)

d1$pass_fail = ifelse(d1$G3>=10,0,1)

library(gbm)
#Boosting
for(k in 1:nfolds){ # k folds
  test_i = which(foldPosition == k)
  d1.test = d1[test_i,]
  d1.train = d1[-test_i,]
  
  d1.boosting=gbm(pass_fail~.-id-G3-pass_fail-grades,data = d1.train,distribution="bernoulli",n.trees=5000,interaction.depth=4)
  boost.probs=predict(d1.boosting,newdata=d1.test,type="response",n.trees=5000)
  boost.pred=rep(0,dim(d1.test)[1])
  boost.pred[boost.probs>=0.5]=1
  perform <- table(boost.pred,d1.test$pass_fail)
  
  Accuracy[k] <- sum(perform[1,1],perform[2,2])/sum(perform[,])
  Recall.No[k] <- perform[1,1]/sum(perform[,1])
  Recall.Yes[k] <- perform[2,2]/sum(perform[,2])
  Precision.No[k] <- perform[1,1]/sum(perform[1,])
  Precision.Yes[k] <- perform[2,2]/sum(perform[2,])
}
Accuracy
mean(Accuracy)