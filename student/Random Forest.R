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

library(tree)
tree.d1=tree(pass_fail~.-id-G3,d1.train)
summary(tree.d1)
plot(tree.d1)
text(tree.d1,cex=0.8)
tree.d1
tree.pred=predict(tree.d1,d1.test,type="class")
table(tree.pred,d1.test$pass_fail)
