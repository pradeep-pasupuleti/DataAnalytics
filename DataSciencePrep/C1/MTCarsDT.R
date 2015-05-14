librarr(rpart)
attach(mtcars)
dim(mtcars)
str(mtcars)
pairs(mtcars)
summary(lm(mpg~.,data=mtcars))

plot(density(c(-20, rep(0,98), 20)), xlim = c(-4, 4))  # IQR = 0
rpart_mt=rpart(mpg~.,method="anova", data=mtcars,control=rpart.control(minsplit=8,cp=0.001))
rpart_mt
plot(rpart_mt,main= "Classification of mtcars", margin=0.15,uniform=TRUE)
text(rpart_mt, use.n=TRUE, all=TRUE, cex=.8,col="blue")
printcp(rpart_mt)
nprunemt=rpart_mt$cptable[which.min(rpart_mt$cptable[,"xerror"]),"CP"]
nprunemt
pmttree<- prune(rpart_mt,cp=0.03)
printcp(pmttree)

plot(pmttree,main= "Classification of pruned pmtree", margin=0.15,uniform=TRUE)
text(pmttree, use.n=TRUE, all=TRUE, cex=.8,col="blue")

par(mfrow=c(1,2)) # two plots on one page
rsq.rpart(pmttree)
rsq.rpart
# R Sqared value
tmp <- printcp(pmttree)
rsq.val <- 1-tmp[,c(3,4)] 
rsq.val

tmp1<-rsq.val[nrow(rsq.val),] 
tmp1

library(randomForest)
set.seed(400)
mt.rf<-randomForest(mpg~.,data=mtcars,ntree=100,keep.forest=TRUE,importance=TRUE, rsq=TRUE)
plot(mt.rf, log="y")
importance(mt.rf)
print(mt.rf$rsq)
plot(mt.rf$rsq)
hist(treesize(mt.rf))
predict(mt.rf)
