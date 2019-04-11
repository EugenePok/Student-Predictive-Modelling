nfolds = 5
foldPosition = sample(rep(1:nfolds, length.out = dim(d2)[1]))

Accuracy = replicate(nfolds,0)
Recall.No = replicate(nfolds,0)
Recall.Yes = replicate(nfolds,0)
Precision.No = replicate(nfolds,0)
Precision.Yes = replicate(nfolds,0)

library(tree)
for(k in 1:nfolds){ # k folds
  test_i = which(foldPosition == k)
  d2.test = d2[test_i,]
  d2.train = d2[-test_i,]
  
  tree.d2=tree(pass_fail~.-id-G3-pass_fail-grades,d2.train)
  tree.pred=predict(tree.d2,d2.test,type="class")
  perform <- table(tree.pred, d2.test$pass_fail)
  
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

#Summary of last tree.
summary(tree.d2)
plot(tree.d2)
text(tree.d2,cex=0.8)
tree.d2
table(tree.pred,d2.test$pass_fail)