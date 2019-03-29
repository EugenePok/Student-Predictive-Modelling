d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d1.length = dim(d1)[1]
id = 1:d1.length
pass_fail = as.factor(d1$G3 >= 10)
d1 = data.frame(id,d1,pass_fail)


nfolds = ceiling(d1.length/10)
foldPosition = sample(rep(1:nfolds, length.out = dim(d1)[1]))

Accuracy = replicate(nfolds,0)
Recall.No = replicate(nfolds,0)
Recall.Yes = replicate(nfolds,0)
Precision.No = replicate(nfolds,0)
Precision.Yes = replicate(nfolds,0)

library(e1071)
for(k in 1:nfolds){ # k folds
  test_i = which(foldPosition == k)
  d1.test = d1[test_i,]
  d1.train = d1[-test_i,]
  
  model.nb <- naiveBayes(pass_fail ~ .-id-G3, data = d1.train)
  pred.nb <- predict(model.nb, newdata = d1.test)
  perform <- table(pred.nb, d1.test$pass_fail)
  
  Accuracy[k] <- sum(perform[1,1],perform[2,2])/sum(perform[,])
  Recall.No[k] <- perform[1,1]/sum(perform[,1])
  Recall.Yes[k] <- perform[2,2]/sum(perform[,2])
  Precision.No[k] <- perform[1,1]/sum(perform[1,])
  Precision.Yes[k] <- perform[2,2]/sum(perform[2,])
}

mean(Accuracy)

