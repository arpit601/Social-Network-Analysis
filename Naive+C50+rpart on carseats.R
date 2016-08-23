library(ISLR)
Carseats
dim(Carseats)
#using library for decision
library(rpart)
library(rpart.plot)

#splitting dataset
trainset<- Carseats[1:320,]
trainset
testset<- Carseats[321:400,]
testset
attach(Carseats)
#Building the decision tree model on trainset #
m2 <- rpart(US~., data=trainset, method="class")

#Plot the decision tree
rpart.plot(m2,type=3,extra=101,fallen.leaves=T)

#USing the above model on testset
p1<- predict(m2,testset,type="class")
p1

#Create the confusion matrix
table(testset[,11],p1)

#

library(C50)

#get the data
Carseats


#splitting dataset
trainset<- Carseats[1:320,]
trainset
testset<- Carseats[321:400,]
testset

#Building the decision tree model on trainset #
m3 <- C5.0(trainset[,-11],trainset[,11])
summary(m3)

#predicting
p4<- predict(m3,testset)
p4

#creating confusion matrix
table(testset[,11],p4)

# 49.4

#get the required package
library(e1071)



#splitting dataset
trainset<- Carseats[1:320,]
trainset
testset<- Carseats[321:1400,]
testset

# BUilding Naive Bayes classifier on dataset #
m5<- naiveBayes(US~.,data=trainset)

#predict 
p5<- predict(m5,testset)
p5

#creating confusion matrix
table(testset[,11],p5)

library(corrplot)
b<- cor(Carseats[,-c(7,10,11)])
corrplot.mixed(b)

