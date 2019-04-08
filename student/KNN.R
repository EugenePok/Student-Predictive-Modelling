library(BBmisc)
d2$age <- normalize(d2$age, method="standardize")
d2$absences <- normalize(d2$absences, method="standardize")
d2$G1 <- normalize(d2$G1, method="standardize")
d2$G2 <- normalize(d2$G2, method="standardize")

library(kknn)
Best_k = replicate(50,0)
Accuracy = replicate(50,0) # Accuracy of each k
nfolds = ceiling(d2.length/10)
foldPosition = sample(rep(1:nfolds, length.out = dim(d2)[1]))
for(k in 1:nfolds){ # k folds
  test_i = which(foldPosition == k)
  d2.test = d2[test_i,]
  d2.train = d2[-test_i,]
  
  for(nn in 1:50){ #no. of neighbor
    d2.kknn = kknn(pass_fail~.-id-G3-pass_fail-grades, d2.train, d2.test, k=nn, kernel = "triangular")
    fit = fitted(d2.kknn)
    perform <- table(fit, d2.test$pass_fail)
    Accuracy[nn] <- sum(perform[1,1],perform[2,2])/sum(perform[,])
  }
  
  Best_K_in_CurrentSet = which(Accuracy==max(Accuracy))
  for(k in Best_K_in_CurrentSet){
    Best_k[k] = Best_k[k] + 1
  }
}

plot(1:50,Best_k,xlab = "No.of.k", ylab= "Frequency")
result = data.frame(Best_k)
write.csv(result,'Best_k_for_KNNd2_2nd_time.csv')

nfolds = 5
j = replicate(10,0)
for(i in 1:10){
  foldPosition = sample(rep(1:nfolds, length.out = dim(d2)[1]))
  
  Accuracy = replicate(nfolds,0)
  Recall.No = replicate(nfolds,0)
  Recall.Yes = replicate(nfolds,0)
  Precision.No = replicate(nfolds,0)
  Precision.Yes = replicate(nfolds,0)
  
  for(k in 1:nfolds){ # k folds
    test_i = which(foldPosition == k)
    d2.test = d2[test_i,]
    d2.train = d2[-test_i,]
    
    d2.kknn = kknn(pass_fail~.-id-G3-G1-pass_fail-grades, d2.train, d2.test, k=12)
    fit = fitted(d2.kknn)
    perform <- table(fit, d2.test$pass_fail)
    Accuracy[k] <- sum(perform[1,1],perform[2,2])/sum(perform[,])
    Recall.No[k] <- perform[1,1]/sum(perform[,1])
    Recall.Yes[k] <- perform[2,2]/sum(perform[,2])
    Precision.No[k] <- perform[1,1]/sum(perform[1,])
    Precision.Yes[k] <- perform[2,2]/sum(perform[2,])
  }
  
  j[i] = mean(Accuracy)
  remove(d2.kknn)
}