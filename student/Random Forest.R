nfolds = ceiling(d1.length/10)
foldPosition = sample(rep(1:nfolds, length.out = dim(d1)[1]))

Accuracy = replicate(nfolds,0)
Recall.No = replicate(nfolds,0)
Recall.Yes = replicate(nfolds,0)
Precision.No = replicate(nfolds,0)
Precision.Yes = replicate(nfolds,0)

library(tree)
for(k in 1:nfolds){ # k folds
  test_i = which(foldPosition == k)
  d1.test = d1[test_i,]
  d1.train = d1[-test_i,]
  
  tree.d1=tree(pass_fail~.-id-G3-pass_fail-grades,d1.train)
  tree.pred=predict(tree.d1,d1.test,type="class")
  perform <- table(tree.pred, d1.test$pass_fail)
  
  Accuracy[k] <- sum(perform[1,1],perform[2,2])/sum(perform[,])
  Recall.No[k] <- perform[1,1]/sum(perform[,1])
  Recall.Yes[k] <- perform[2,2]/sum(perform[,2])
  Precision.No[k] <- perform[1,1]/sum(perform[1,])
  Precision.Yes[k] <- perform[2,2]/sum(perform[2,])
}
mean(Accuracy)

#Summary of last tree.
summary(tree.d1)
plot(tree.d1)
text(tree.d1,cex=0.8)
tree.d1
table(tree.pred,d1.test$pass_fail)