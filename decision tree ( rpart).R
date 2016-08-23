#using library for decision
library(rpart)
library(rpart.plot)
#get the data
iris
#shuffling because my class was not random
set.seed(9850)
g<- runif(nrow(iris))
irisr<- iris[order(g),]
irisr

#splitting dataset
trainset<- irisr[1:60,]
trainset
testset<- irisr[61:150,]
testset
attach(iris)
#Building the decision tree model on trainset #
m <- rpart(Species~., data=trainset, method="class")

#Plot the decision tree
rpart.plot(m,type=3,extra=101,fallen.leaves=T)

#USing the above model on testset
p<- predict(m,testset,type="class")
p

#Create the confusion matrix
table(testset[,5],p)



library(C50)

#get the data
iris
#shuffling because my class was not random
set.seed(9850)
g<- runif(nrow(iris))
irisr<- iris[order(g),]
irisr

#splitting dataset
trainset<- irisr[1:100,]
trainset
testset<- irisr[101:150,]
testset

#Building the decision tree model on trainset #
m1 <- C5.0(trainset[,-5],testset[,5])
summary(m1)

#predicting
p1<- predict(m1,testset)
p1

#creating confusion matrix
table(testset[,5],p1)