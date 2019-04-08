setwd("D:/Git/Student-Predictive-Modelling/student")
d1=read.table("student-mat.csv",sep=";",header=TRUE)
d2=read.table("student-por.csv",sep=";",header=TRUE)

d1.length = dim(d1)[1]
id = 1:d1.length
pass_fail = as.factor(d1$G3 >= 10)
d1 = data.frame(id,d1,pass_fail)
d1$grades <- cut(d1$G3, breaks=c(-1,9,11,13,15,20),labels=c("F","D","C","B","A"))
colnames(d1)
sapply(d1,class)


