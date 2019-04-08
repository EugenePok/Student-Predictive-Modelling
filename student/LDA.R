nfolds = 5
foldPosition = sample(rep(1:nfolds, length.out = dim(d1)[1]))

Accuracy = replicate(nfolds,0)
Recall.No = replicate(nfolds,0)
Recall.Yes = replicate(nfolds,0)
Precision.No = replicate(nfolds,0)
Precision.Yes = replicate(nfolds,0)

# LDA
library(MASS)
for(k in 1:nfolds){ # k folds
  test_i = which(foldPosition == k)
  d1.test = d1[test_i,]
  d1.train = d1[-test_i,]
  
  d1.lda=lda(pass_fail~.-id-G3-pass_fail-grades, data = d1.train)
  d1.pred <- predict(d1.lda,newdata = d1.test)
  perform <- table(d1.pred$class, d1.test$pass_fail)
  
  Accuracy[k] <- sum(perform[1,1],perform[2,2])/sum(perform[,])
  Recall.No[k] <- perform[1,1]/sum(perform[,1])
  Recall.Yes[k] <- perform[2,2]/sum(perform[,2])
  Precision.No[k] <- perform[1,1]/sum(perform[1,])
  Precision.Yes[k] <- perform[2,2]/sum(perform[2,])
}

mean(Accuracy)
