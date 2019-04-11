set.seed(123)
setwd("D:/Git/Student-Predictive-Modelling/student")
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d1.length = dim(d1)[1]
id = 1:d1.length
pass_fail = as.factor(d1$G3 >= 10)
d1 = data.frame(id,d1,pass_fail)
colnames(d1)
sapply(d1,class)

d2.length = dim(d2)[1]
id = 1:d2.length
pass_fail = as.factor(d2$G3 >= 10)
d2 = data.frame(id,d2,pass_fail)
colnames(d2)
sapply(d2,class)