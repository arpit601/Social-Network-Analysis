library(ISLR)

#using library for decision
library(rpart)
library(rpart.plot)
#get the data
Smarket
dim(Smarket)

#splitting dataset
trainset<- Smarket[1:1000,]
trainset
testset<- Smarket[1001:1250,]
testset
attach(Smarket)
#Building the decision tree model on trainset #
m2 <- rpart(Direction~., data=trainset, method="class")

#Plot the decision tree
rpart.plot(m2,type=3,extra=101,fallen.leaves=T)

#USing the above model on testset
p1<- predict(m2,testset,type="class")
p1

#Create the confusion matrix
table(testset[,9],p1)

#100% for 80/20

library(C50)

#get the data
Smarket


#splitting dataset
trainset<- Smarket[1:1000,]
trainset
testset<- Smarket[1001:1250,]
testset

#Building the decision tree model on trainset #
m3 <- C5.0(trainset[,-9],trainset[,9])
summary(m3)

#predicting
p4<- predict(m3,testset)
p4

#creating confusion matrix
table(testset[,9],p4)

# 49.4

#get the required package
library(e1071)



#splitting dataset
trainset<- Smarket[1:1000,]
trainset
testset<- Smarket[1001:1250,]
testset

# BUilding Naive Bayes classifier on dataset #
m5<- naiveBayes(Direction~.,data=trainset)

#predict 
p5<- predict(m5,testset)
p5

#creating confusion matrix
table(testset[,9],p5)

library(corrplot)
b<- cor(Smarket[,-9])
corrplot.mixed(b)

