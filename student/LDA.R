nfolds = 5
foldPosition = sample(rep(1:nfolds, length.out = dim(d2)[1]))

Accuracy = replicate(nfolds,0)
Recall.No = replicate(nfolds,0)
Recall.Yes = replicate(nfolds,0)
Precision.No = replicate(nfolds,0)
Precision.Yes = replicate(nfolds,0)

# LDA
library(MASS)
for(k in 1:nfolds){ # k folds
  test_i = which(foldPosition == k)
  d2.test = d2[test_i,]
  d2.train = d2[-test_i,]
  
  d2.lda=lda(pass_fail~.-id-G3-G2-G1-pass_fail-grades, data = d2.train)
  d2.pred <- predict(d2.lda,newdata = d2.test)
  perform <- table(d2.pred$class, d2.test$pass_fail)
  
  Accuracy[k] <- sum(perform[1,1],perform[2,2])/sum(perform[,])
  Recall.No[k] <- perform[1,1]/sum(perform[,1])
  Recall.Yes[k] <- perform[2,2]/sum(perform[,2])
  Precision.No[k] <- perform[1,1]/sum(perform[1,])
  Precision.Yes[k] <- perform[2,2]/sum(perform[2,])
}
mean(Accuracy)
mean(Recall.Yes)
mean(Recall.No)
mean(Precision.No)
mean(Precision.Yes)