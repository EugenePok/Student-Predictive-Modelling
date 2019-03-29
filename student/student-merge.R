d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)
#d3=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
#print(nrow(d3)) # 382 students

dim(d3)
# There are 382 observations with 53 variables

id = 1:dim(d1)[1]
pass_fail = as.factor(d1$G3 >= 10)
d1 = data.frame(id,d1,pass_fail)
library(splitstackshape)
#d1.train <- stratified(d1,"pass_fail",size=0.7)
library(dplyr)
#d1.test <- anti_join(d1, d1.train, by="id")
#View(d1.test)


library(kknn)
x <- 1:40
r <- 1:1000
Best_k = replicate(40,0)
Accuracy = replicate(40,0) # Accuracy of each k
for(re in r){
d1.train <- stratified(d1,"pass_fail",size=0.7)
d1.test <- anti_join(d1, d1.train, by="id")
for(val in x){
d1.kknn = kknn(pass_fail~.-id-G3, d1.train, d1.test, k=val, kernel = "triangular")
#summary(d1.kknn)
fit = fitted(d1.kknn)
perform <- table(fit, d1.test$pass_fail)
Accuracy[val] <- sum(perform[1,1],perform[2,2])/sum(perform[,])
}
current_k = which(Accuracy==max(Accuracy))
for(k in current_k){
Best_k[k] = Best_k[k] + 1
}
}
plot(x,Best_k,xlab = "k", ylab= "Best_k")
Best_k
result = data.frame(x,Best_k)
write.csv(result,'Best_k_for_KNN.csv')

## TDL : cross validation
## TD Model : Logistic, Bagging, Random Forest, naives bayes
