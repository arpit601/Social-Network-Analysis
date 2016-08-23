#get the required package
library(e1071)

#get the data
iris

#shuffling the data
set.seed(9850)
g<- runif(nrow(iris))
irisr<- iris[order(g),]
irisr

#splitting dataset
trainset<- irisr[1:100,]
trainset
testset<- irisr[101:150,]
testset

# BUilding Naive Bayes classifier on dataset #
m <- naiveBayes(Species~.,data=trainset)

#predict 
p2<- predict(m,testset)
p2

#creating confusion matrix
table(testset[,5],p2)


#checking whether features are independent or not
library(corrplot)
a<- cor(iris[,-5])
corrplot.mixed(a)

#scatter plot
pairs(iris[,-5])

library(ggvis)
iris%>%ggvis(~Sepal.Length,~Sepal.Width,fill=~Species)%>%layer_points()
iris%>%ggvis(~Petal.Length,~Petal.Width,fill=~Species)%>%layer_points()

library(ggplot2)
p<-ggplot(iris, aes(x=Sepal.Length,y=Sepal.Width)) + geom_point(aes(color=Species))
p