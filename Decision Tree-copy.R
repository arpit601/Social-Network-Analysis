#Classification tree with rpart
library(rpart)
library(party)
kyphosis
attach(kyphosis)
head(kyphosis)
?kyphosis

#GROW TREE

fit <- rpart(Kyphosis~ Age+Number+Start,method = "class", data = kyphosis)

#PLOT TREE
plot(fit,uniform=TRUE,main="Classification Tree for Kyphosis")
text(fit,use.n=TRUE,all=TRUE,cex=0.8)

library(party)
#reading skills data
readingSkills
RS=readingSkills
attach(RS)
#give the chart file a name
png(file="decision_tree.png")

#Create the tree
output.tree<- ctree(nativeSpeaker~age+shoeSize+score,data = RS)

#plot the tree
plot(output.tree)

CS<- read.csv(file.choose())
attach(CS)
head(CS)

output<- ctree(Computer.bought~CS$Age+Income+Student+Credit.rating,data = CS)
plot(output)


library(RWeka)
##################################################################
# decision tree for insurance data using RWeka
#reading data into R
IB <- read.csv(file.choose())
head(IB)
colnames(IB) <- c("Gender", "Marital_Status", "Income_Level", "Insurance_Bought")
head(IB)
library(RWeka)
library(partykit)
attach(IB)
IB_fit <- J48(Insurance_Bought~Gender+ Marital_Status+Income_Level,data = IB, control = Weka_control(),options = NULL)
plot(IB_fit)


################################################################
#now generating using party (ctree)
library(party)
jpeg(file= "insurance_bought.jpeg")
fitting<- ctree(Insurance_Bought~Gender+Marital_Status+Income_Level, data = IB)
plot(fitting)

library(rpart)
library(party)
