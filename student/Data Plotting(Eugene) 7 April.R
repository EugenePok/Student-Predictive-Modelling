# The function is to slice & manipulate the data in 
# simple way.
library(dplyr)

# Ploting library
library(ggplot2)
library(ggthemes)
library(colorspace)
library(GGally) 

# For "ggcorr" use
#Setworking Directory
# Read Data
# setwd("C:/Users/User/Desktop/(Uni)2019Jan/(Uni) 2019 Jan Assignment/Predictive Modelling Assignment")
# d1=read.table("student-mat.csv",sep=";",header=TRUE)
# d2=read.table("student-por.csv",sep=";",header=TRUE)

student.mat=read.table("student-mat.csv",sep=";",header=TRUE)
student.por=read.table("student-por.csv",sep=";",header=TRUE)
student=merge(student.mat,student.por,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

# student<-read.table("student-mat-por.csv",sep=",",header=TRUE)

#######################
#if(TRUE){
#Make G3 as binary variable
student[which(student$G3.x<10),"G3.x"]<-0
student[which(student$G3.x>=10),"G3.x"]<-1
student$G3.x<-as.factor(student$G3.x)

student[which(student$G3.y<10),"G3.y"]<-0
student[which(student$G3.y>=10),"G3.y"]<-1
student$G3.y<-as.factor(student$G3.y)

#}
#######################
##### 1. Data Prep. & Exploration ####
#.i View Data
names(student) # Check Var. name
str(student) # Check Var. class
ncol(student)

######## Check for missing variable ###########
### Check for overal
sum(is.na(student))# No NA exist 
sum(student=="") # No data have empty string
### Use it for checking Missing Data in each variable, 
### since the data dont have missing variable, we no need use these tow lines
sapply(student,function(x) sum(is.na(x)))
sapply(student,function(x) sum(x==""))
##########

##### Change variable type ####
factor_variable <- c("Medu","Fedu",
                     "traveltime.x","studytime.x","failures.x",
                     "famrel.x","freetime.x","goout.x",
                     "Dalc.x","Walc.x","health.x",
                     "traveltime.y","studytime.y","failures.y",
                     "famrel.y","freetime.y","goout.y",
                     "Dalc.y","Walc.y","health.y"
)
student[factor_variable] <- lapply(student[factor_variable], factor)
sapply(student,class) # Check the class for each variable


############## Analysis start ###########


############ Individual variable distribution checking ###############
library(data.table)
setDT(student) # Set as data.table to use the function in library "data.table"
student[,.N,by=list(G3.x)][,perc:=100*N/sum(N)] %>% 
  ggplot(aes(x=G3.x,y=perc)) +
  geom_bar(stat="identity", width = 0.7)+
  labs(x = "G3 Result(Math.)", y = "percent")+
  ggtitle("Distribution in G3 Result(Math.)")+
  theme_minimal(base_size = 14)

student[,.N,by=list(G3.y)][,perc:=100*N/sum(N)] %>% 
  ggplot(aes(x=G3.y,y=perc)) +
  geom_bar(stat="identity", width = 0.7)+
  labs(x = "G3 Result(Por.)", y = "percent")+
  ggtitle("Distribution in G3 Result(Por.)")+
  theme_minimal(base_size = 14)

# Comment: 
# It is super biased for G3 Result in Por.
# Some data treatment is recommended for the variable
# like upsampling , downsampling, or using some special model to fit this kind variable
# Some example use cases is credit scoring, deafault loan etc.

setDF(student) # Set back to data.frame format

############# Individual and Response variable relation checking

######## Factor Variable Group ##################
# Percentage bar plot 
# Desc.:
# To if there any variable that make the 0 and 1 differ significantly

# Comment on G3. Mathematic:
# No significant factor level could affected the response variable greatly
student%>% 
  group_by(Mjob,G3.x) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))%>%
  ggplot(aes(x = Mjob, y = perc*100, fill = G3.x)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Mjob", y = "percent", fill = "G3.x") +
  ggtitle("How Mjob impacts G3 Result(Math.)")+
  theme_minimal(base_size = 14)

student%>% 
  group_by(Fjob,G3.x) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count))%>%
  ggplot(aes(x = Fjob, y = perc*100, fill = G3.x)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Fjob", y = "percent", fill = "G3.x") +
  ggtitle("How Fjob impacts G3 Result(Math.)")+
  theme_minimal(base_size = 14)

###### About the portuguese G3 result ####### 
# PS: Just for you guys to take alook on how biased data lead to a biased conclusion
# The relatively percentage in each factor level have significant different
# but this doesnot mean the factor is good in predict the G3.y

## Information Value is recomended to extracted out most significant factor

student%>% 
  group_by(Mjob,G3.y) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>%
  ggplot(aes(x = Mjob, y = perc*100, fill = G3.y)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Mjob", y = "percent", fill = "G3.y") +
  ggtitle("How Mjob impacts G3 Result(Por.)")+
  theme_minimal(base_size = 14)

student%>% 
  group_by(Fjob,G3.y) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>%
  ggplot(aes(x = Fjob, y = perc*100, fill = G3.y)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Fjob", y = "percent", fill = "G3.y") +
  ggtitle("How Fjob impacts G3 Result(Por.)")+
  theme_minimal(base_size = 14)
# If you just look on the graph, definitely you will think all student's father 
# that work at home will pass the result.
# But this cause by the number of Fjob's at_home is low, and with the unbalance
# data exist in G3.y, this make us think that if father stay at home, the student 
# will pass the Portuguese Exam

# Age vs G3. Math,
student%>% 
  group_by(age,G3.x) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>%
  ggplot(aes(x = age, y = perc*100, fill = G3.x)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Age", y = "percent", fill = "G3.x") +
  ggtitle("How Age impacts G3 Result(Math.)")+
  theme_minimal(base_size = 14)

ggplot(data = student,mapping = aes(x=age,y=..count..,fill=G3.x))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("Age")+
  ylab("Count")+
  ggtitle("How Age impacts G3 Result(Math.)")+
  geom_text(stat = "count",aes(label=..count..),position = position_dodge(width = 1),vjust=-.6)+
  theme_few()

# Age vs G3. Portuguese
student%>% 
  group_by(age,G3.y) %>% 
  summarise(count=n()) %>% 
  mutate(perc=count/sum(count)) %>%
  ggplot(aes(x = age, y = perc*100, fill = G3.y)) +
  geom_bar(stat="identity", width = 0.7) +
  labs(x = "Age", y = "percent", fill = "G3.y") +
  ggtitle("How Age impacts G3 Result(Port.)")+
  theme_minimal(base_size = 14)

ggplot(data = student,mapping = aes(x=age,y=..count..,fill=G3.y))+
  geom_bar(stat = "count",position = "dodge")+
  xlab("Age")+
  ylab("Count")+
  ggtitle("How Age impacts G3 Result(Port.)")+
  geom_text(stat = "count",aes(label=..count..),position = position_dodge(width = 1),vjust=-.6)+
  theme_few()

######## Integer Variable Group ##################



# Temporary change the response variable to integer type
student$G3.x<-as.integer(student$G3.x)
student$G3.y<-as.integer(student$G3.y)

student %>% 
  select(which(sapply(.,is.integer))) %>%  # Select all integer class variable
  ggcorr(nbreaks = 4, palette = "RdGy", label = TRUE, 
         label_size = 3, label_color = "white")
# Comment:
# As we can see G1.x and G2.x have 90% correlation, 
# and both of them have about 70% correlation with  G3.X,
# which indicate both of them have better prediction on G3.x

# While the g3.y  correlation with g1.y and g2.y is not significant
# we suspect is the biased data problem, some transformation or
# sampling technique is recomended before model building


# Change back the response variable to factor type
student$G3.x<-as.factor(student$G3.x)
student$G3.y<-as.factor(student$G3.y)

