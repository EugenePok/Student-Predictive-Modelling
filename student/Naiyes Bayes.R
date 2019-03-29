d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

id = 1:dim(d1)[1]
pass_fail = as.factor(d1$G3 >= 10)
d1 = data.frame(id,d1,pass_fail)
library(splitstackshape)
d1.train <- stratified(d1,"pass_fail",size=0.7)
library(dplyr)
d1.test <- anti_join(d1, d1.train, by="id")
#View(d1.test)

library(e1071)
model.nb <- naiveBayes(pass_fail ~ .-id-G3, data = d1.train)
pred.nb <- predict(model.nb, newdata = d1.test)
perform <- table(pred.nb, d1.test$pass_fail)

# performance
Accuracy <- sum(perform[1,1],perform[2,2])/sum(perform[,])
Recall.0 <- perform[1,1]/sum(perform[,1])
Recall.1 <- perform[2,2]/sum(perform[,2])
Precision.0 <- perform[1,1]/sum(perform[1,])
Precision.1 <- perform[2,2]/sum(perform[2,])
