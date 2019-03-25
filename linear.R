setwd("D:\\jigsaw\\bc1\\Class Datasets")
a<-read.csv("expenses.csv", header = T, stringsAsFactors = T)
dim(a)
str(a)
View(a)
summary(a)
summary(a$age)

CHEKING FOR OUTLIERS IN aGE

x<-boxplot(a$age)
x
out<-x$out
out
index<-which(a$age %in% x$out)
index
a$age[index]<-39
View(a)

x<-boxplot(a$bmi)
x
out<-x$out
out
index<-which(a$bmi %in% x$out)
index
a$bmi[index]<-39
View(a)
#droppind na values in age and  bmi
index<-which(is.na(a$bmi))
a1<-a[-index,]
View(a1)
summary(a1)


#building model

reg<-lm(charges~age+sex+bmi+children+smoker+region,a1)
reg
summary(reg)
reduced <- step(reg,direction = "backward")




Mulreg<-lm(charges~age+sex+bmi+children+smoker+region,a1)
Mulreg
summary(Mulreg)
reduced <- step(reg,direction = "backward")







