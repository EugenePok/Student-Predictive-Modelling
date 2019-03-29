d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)
#d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

dim(d1)
# There are 395 observations with 33 variables

d1.length = dim(d1)[1]
id = 1:d1.length
pass_fail = as.factor(d1$G3 >= 10)
d1 = data.frame(id,d1,pass_fail)

library(kknn)
Best_k = replicate(50,0)
Accuracy = replicate(50,0) # Accuracy of each k
nfolds = ceiling(d1.length/10)
foldPosition = sample(rep(1:nfolds, length.out = dim(d1)[1]))
for(k in 1:nfolds){ # k folds
  test_i = which(foldPosition == k)
  d1.test = d1[test_i,]
  d1.train = d1[-test_i,]
  
  for(nn in 1:50){ #no. of neighbor
    d1.kknn = kknn(pass_fail~.-id-G3, d1.train, d1.test, k=nn, kernel = "triangular")
    fit = fitted(d1.kknn)
    perform <- table(fit, d1.test$pass_fail)
    Accuracy[nn] <- sum(perform[1,1],perform[2,2])/sum(perform[,])
  }
  
  Best_K_in_CurrentSet = which(Accuracy==max(Accuracy))
  for(k in Best_K_in_CurrentSet){
    Best_k[k] = Best_k[k] + 1
  }
}

plot(1:50,Best_k,xlab = "k", ylab= "Best_k")
result = data.frame(1:50,Best_k)
write.csv(result,'Best_k_for_KNN.csv')

nfolds = ceiling(d1.length/10)
foldPosition = sample(rep(1:nfolds, length.out = dim(d1)[1]))

Accuracy = replicate(nfolds,0)
Recall.No = replicate(nfolds,0)
Recall.Yes = replicate(nfolds,0)
Precision.No = replicate(nfolds,0)
Precision.Yes = replicate(nfolds,0)

for(k in 1:nfolds){ # k folds
  test_i = which(foldPosition == k)
  d1.test = d1[test_i,]
  d1.train = d1[-test_i,]

  d1.kknn = kknn(pass_fail~.-id-G3, d1.train, d1.test, k=31, kernel = "triangular")
  fit = fitted(d1.kknn)
  perform <- table(fit, d1.test$pass_fail)
  Accuracy[k] <- sum(perform[1,1],perform[2,2])/sum(perform[,])
  Recall.No[k] <- perform[1,1]/sum(perform[,1])
  Recall.Yes[k] <- perform[2,2]/sum(perform[,2])
  Precision.No[k] <- perform[1,1]/sum(perform[1,])
  Precision.Yes[k] <- perform[2,2]/sum(perform[2,])
}

mean(Accuracy)
## TD Model : Logistic, Bagging, Random Forest, naives bayes
