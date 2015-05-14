library(rpart)
attach(cu.summary)
attributes(cu.summary)
head(cu.summary,n=10)
dim(cu.summary)
rpart_cars=rpart(Mileage~.,method="anova", data=cu.summary,control=rpart.control(minsplit=8,cp=0.01))
rpart_cars
plot(rpart_cars,main= "Classification of mtcars", margin=0.15,uniform=TRUE)

text(rpart_cars, use.n=TRUE, all=TRUE, cex=.8,col="blue")

printcp(rpart_cars)
carnprune=rpart_cars$cptable[which.min(rpart_cars$cptable[,"xerror"]),"CP"]
carnprune
plotcp(rpart_cars)
rpart_cars$cptable
prunedcar=prune(rpart_cars,cp=carnprune)
prunedcar
printcp(prunedcar)
plotcp(prunedcar)
plot(prunedcar,main= "Classification of mtcars", margin=0.15,uniform=TRUE)
text(prunedcar, use.n=TRUE, all=TRUE, cex=.8,col="blue")
