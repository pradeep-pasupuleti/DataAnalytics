library(rpart)
library(FSelector)

attach(kyphosis)
dim(kyphosis)
summary(kyphosis)
str(kyphosis)
head(kyphosis,n=10)
rpart_kyph=rpart(Kyphosis~.,method="class", data=kyphosis,control=rpart.control(minsplit=8,cp=0.01))
rpart_kyph
plot(rpart_kyph,main= "Classification of kyphosis", margin=0.15,uniform=TRUE)
text(rpart_kyph, use.n=TRUE, all=TRUE, cex=.8,col="blue")

"""kyweights <- information.gain(kyphosis~., kyphosis)
print(kyweights)
subset <- cutoff.k(kyweights, 4)
kyf <- as.simple.formula(subset, "kyphosis")
print(kyf)"""
printcp(rpart_kyph)
"""The cptable provides a brief summary of the overall fit of the model. The table is printed
from the smallest tree (0 splits) to the largest one (7 splits). CPtable always lists number of
splits and not the number of nodes (which is 1+the number of splits)."""


nprune=rpart_kyph$cptable[which.min(rpart_kyph$cptable[,"xerror"]),"CP"]
nprune
#pfit<- prune(fit, cp=   fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"])

ptree<- prune(rpart_kyph,cp=rpart_kyph$cptable[which.min(rpart_kyph$cptable[,"xerror"]),"CP"])
//ptree<- prune(rpart_kyph,cp=0.176)
printcp(ptree)
plot(ptree,main= "Classification of kyphosis", margin=0.15,uniform=TRUE)
text(ptree, use.n=TRUE, all=TRUE, cex=.8,col="blue")

