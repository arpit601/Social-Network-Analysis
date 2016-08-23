library(mlbench)
data("HouseVotes84")
?HouseVotes84
head(HouseVotes84)

#using library for decision
library(rpart)
library(rpart.plot)
#get the data
HouseVotes84

dim(HouseVotes84)
#splitting dataset
trainset<- HouseVotes84[1:300,]
trainset
testset<- HouseVotes84[301:435,]
testset
attach(HouseVotes84)
#Building the decision tree model on trainset #
m2 <- rpart(Class~., data=trainset, method="class")

#Plot the decision tree
rpart.plot(m2,type=3,extra=101,fallen.leaves=T)

#USing the above model on testset
p1<- predict(m2,testset,type="class")
p1

#Create the confusion matrix
table(testset[,1],p1)

#88% for 80/20
#92.5 for 70/30

library(C50)

#get the data
HouseVotes84


#splitting dataset
trainset<- HouseVotes84[1:300,]
trainset
testset<- HouseVotes84[301:435,]
testset

#Building the decision tree model on trainset #
m3 <- C5.0(trainset[,-1],testset[,1])
summary(m3)

#predicting
p4<- predict(m3,testset)
p4

#creating confusion matrix
table(testset[,1],p4)

# 49.4

#get the required package
library(e1071)

#get the data
HouseVotes84


#splitting dataset
trainset<- HouseVotes84[1:348,]
trainset
testset<- HouseVotes84[349:435,]
testset

# BUilding Naive Bayes classifier on dataset #
m5<- naiveBayes(Class~.,data=trainset)

#predict 
p5<- predict(m5,testset)
p5

#creating confusion matrix
table(testset[,1],p5)

#83.9% for 80/20

# logistic for HOusevotes84
Strain<- HouseVotes84[1:300,]
Stest<- HouseVotes84[301:435,]
Class_te<- Stest$Class
pairs(HouseVotes84)
head(HouseVotes84)
glm_mod <- glm(Class~.,data = Strain,family = binomial(link = "logit"))
summary(glm_mod)

library(car)
vif(glm_mod)
Class_pred_mod<- predict(glm_mod,Stest,type="response")

Direction_test<- rep("Democrat",135)
Direction_test[Class_pred_mod>0.5]="Rebublic"

table(Class_te,Direction_test)

is.na(HouseVotes84)
summary(factor(is.na(HouseVotes84)))
l<-HouseVotes84[!complete.cases(HouseVotes84),]
dim(l)