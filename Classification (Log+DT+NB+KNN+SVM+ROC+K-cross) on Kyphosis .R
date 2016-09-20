################################################################################
############1. DECISION TREE WITH AUC , K-CROSS and CONFUSION MATRIX#####################
#Classification tree with rpart
library(rpart)# for creating decision tree
library(rpart.plot) # for plotting decision tree
library(ROCR) # for finding AUC
library(e1071)
library(class)
#get the data
kyphosis

#top few rows of data
head(kyphosis)

# to know about the data 
?kyphosis

#shuffling the data ( could be done in case my class is not random)
set.seed(9850)
g<- runif(nrow(kyphosis))
kyphosis1<- kyphosis[order(g),]
kyphosis1

#attach kyphosis in global environment
attach(kyphosis1)

# dimensions in data
dim(kyphosis1)

#splitting the dataset
trainset<- kyphosis1[1:62,]
trainset
testset<- kyphosis1[63:81,]
testset

#Building the decision tree model on trainset using rpart(####use this #####)
m <- rpart(Kyphosis~., data=trainset, method="class")
#building the decision tree using C5.0
m50<- C5.0(trainset[,-1],trainset[,1])
summary(m50)
plot(m50)
#Plot the decision tree
rpart.plot(m,type=3,extra=101,fallen.leaves=T)

#Using the above model on testset to predict ( here output will be class to create confusion matrix)
p<- predict(m,testset,type="class")
p

#Using the above model on testset to predict ( here output will be probabilities to find AUC)
p1<- predict(m,testset,type="prob")
p1

#Create the confusion matrix
table(testset[,1],p)

# Building ROC curve and finding AUC on Decision Tree 
library(ROCR)

# use prediction to convert into standardised format
pred<- prediction(p1[,1],testset[,1])
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,lty=2)
AUClog2<- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC:",AUClog2)

######finding accuracy using K-CROSS validation method############
library(caret)
library(rpart)
library(kernlab)

kyphosis1
attach(kyphosis1)
# define training control
train_control<- trainControl(method = "cv", number=10)
##########method##########
## here i can change method to "loocv" also
train_control

#train the model
model <- train(Kyphosis~., data = kyphosis,trControl=train_control,method="rpart")

###########Method#####################
## rpart method for decision tree
#####################################

#summarize the results for k-cross validation accuracy
print(model)


##############################################################################
########2. NAIVE BAYES CLASSIFIER WITH AUC, K-CROSS and CONFUSION MATRIX #############
library(e1071)

###################################################################
#before building the naive bayes check the correlation matrix########
#####################################################################
library(corrplot)
v<- cor(trainset[,-1])
corrplot(v, method = "number")

# Building Naive Bayes classifier on dataset #
m5<- naiveBayes(Kyphosis~.,data=trainset)
m5

#predict using model to create confusion matrix 
p2<- predict(m5,testset,type = "class")
p2

#predict using model to create ROC and find AUC
p3<- predict(m5,testset,type = "raw")
p3

# create confusion matrix 
table(p2,testset[,1])

### Building ROC curve and AUC using ROCR package
pred<- prediction(p3[,2],testset[,1])
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,lty=2)
AUClog2<- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC:",AUClog2)

######finding accuracy using K-CROSS validation method############
library(caret)
library(rpart)
library(kernlab)

kyphosis1
attach(kyphosis1)
# define training control
train_control1<- trainControl(method = "cv", number=10)
##########method##########
## here i can change method to "loocv" also
train_control1

#train the model
model1 <- train(Kyphosis~., data = kyphosis,trControl=train_control1,method="nb")

###########Method#####################
## nb method for naivebayes
#####################################

#summarize the results for k-cross validation accuracy
print(model1)

##############################################################################
########3. SVM CLASSIFIER WITH AUC and CONFUSION MATRIX #############
#building the SVM model on trainset 
m6<- svm(trainset[,2:4], trainset[,1], probability = TRUE)
summary(m6)

# Using above model on testset ( in order to find AUC)
p4<- predict(m6,testset[,2:4])
p4

# Using above model on testset ( in order to find AUC)
p6<- predict(m6,testset[,2:4], probability = TRUE)
p6
p7<- attributes(p6)
p7<- p7$probabilities
p7

# create confusion matrix 
table(testset[,1],p4)

### Building ROC curve and AUC
pred<- prediction(p7[,2],testset[,1])
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,lty=2)
AUClog2<- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC:",AUClog2)

######finding accuracy using K-CROSS validation method############
library(caret)
library(rpart)
library(kernlab)

kyphosis1
attach(kyphosis1)
# define training control
train_control2<- trainControl(method = "cv", number=10)
##########method##########
## here i can change method to "loocv" also
train_control2

#train the model
model2 <- train(Kyphosis~., data = kyphosis,trControl=train_control2,method="svmLinear2")

###########Method#####################
## svmLinear2 method for svm 
#####################################

#summarize the results for k-cross validation accuracy
print(model2)


##############################################################################
########4. KNN CLASSIFIER WITH AUC, K-CROSS and CONFUSION MATRIX #############
m7 <- knn(trainset[,2:4],testset[,2:4], cl=trainset[,1],k=3,prob = TRUE)
summary(m7)
attributes(m7)
m8<- attributes(m7)
m8<- m8$prob
m11<- 1-m8 # use it depending upon ROC value
#creating confusion matrix
table(testset[,1],m7)

### Building ROC curve and AUC
library(ROCR)
pred<- prediction(m8,testset[,1])# change value depending upon ROC value whether m8 or m11
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,lty=2)
AUClog2<- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC:",AUClog2)

######finding accuracy using K-CROSS validation method############
library(caret)
library(rpart)
library(kernlab)

kyphosis1
attach(kyphosis1)
# define training control
train_control3<- trainControl(method = "cv", number=10)
##########method##########
## here i can change method to "loocv" also
train_control3

#train the model
model3 <- train(Kyphosis~., data = kyphosis,trControl=train_control3,method="knn")

###########Method#####################
## knn method for knn
#####################################

#summarize the results for k-cross validation accuracy
print(model3)
##############################################################################
########5. LOGISTIC REGRESSION WITH AUC and CONFUSION MATRIX #############
# building logistic on kyphosis
m10<- glm(Kyphosis~. , data = trainset, family = binomial(link = "logit"))

# prediction
p8 <- predict(m10, testset, type = "response")
p8

#predicting values using prediction
kyphosis_pred <- rep("absent",19)
kyphosis_pred[p8>0.5]<- "present"
kyphosis_pred

#confusion matrix
table(testset[,1],kyphosis_pred)

### Building ROC curve and AUC
pred<- prediction(p8,testset[,1])
perf<- performance(pred,"tpr","fpr")
plot(perf)
abline(0,1,lty=2)
AUClog2<- performance(pred,measure = "auc")@y.values[[1]]
cat("AUC:",AUClog2)

######finding accuracy using K-CROSS validation method############
library(caret)
library(rpart)
library(kernlab)

kyphosis1
attach(kyphosis1)
# define training control
train_control4<- trainControl(method = "cv", number=10)
##########method##########
## here i can change method to "loocv" also
train_control4

#train the model
model4 <- train(Kyphosis~., data = kyphosis,trControl=train_control4,method="glm")

###########Method#####################
## glm method for logistic
#####################################

#summarize the results for k-cross validation accuracy
print(model4)
