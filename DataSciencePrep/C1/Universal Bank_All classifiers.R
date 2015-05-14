#setwd("D:\\ISEWEB\\Project-related\\ISE-Content\\B3-Jul-Oct-2012\\r-code\\Week4")

univ = read.table('UniversalBank.csv', header=T, sep=',', col.names=c('ID','Age','Exp','Inc','Zip','Family', 
                                                                      'CCAvg','Edu','Mortgage','Loan','Secact','CDact','Web','CC')) 


sum(is.na(univ))
summary(univ)

attach(univ)
hist(Exp)
univ$Exp=cut(univ$Exp, breaks=5, labels =FALSE, ordered_result = TRUE)
hist(Age)
univ$Age=cut(univ$Age, breaks=5, labels =FALSE, ordered_result = TRUE)
hist(Inc)
univ$Inc=cut(univ$Inc, breaks=5, labels =FALSE, ordered_result = TRUE)

hist(CCAvg)

library(infotheo)
univsubset=subset(univ, select=c(CCAvg))
univsubset = discretize(univsubset, disc="equalfreq", nbins=5 )
colnames(univsubset) = c( "CCAvg")

hist(Mortgage)
univ$Mortgage=cut(univ$Mortgage, breaks=5, labels = FALSE, ordered_result = TRUE)

univsubset2=subset(univ, select=-c(ID, Zip, CCAvg))
univ=cbind(univsubset2,univsubset) 

attach(univ)
univ$Exp=as.factor(Exp)
univ$Family=as.factor(Family)
univ$Edu=as.factor(Edu)
univ$Loan=as.factor(Loan)
univ$Secact=as.factor(Secact)
univ$CDact=as.factor(CDact)
univ$Web=as.factor(Web)
univ$CC=as.factor(CC)
univ$Age=as.factor(Age)
univ$Inc=as.factor(Inc)
univ$CCAvg=as.factor(CCAvg)
univ$Mortgage=as.factor(Mortgage)

summary(univ)

train = sample(1:5000,3000) 
univ_train = univ[train,] 
test = (1:5000) [-train] 
univ_test = univ[test,] 


table(univ$Loan)
table(univ_train$Loan)
table(univ_test$Loan)

#Naive Bayes
library(e1071)

modelnb = naiveBayes(Loan ~ ., data = univ_train)
table(univ_train$Loan, predict(modelnb, univ_train))
pred=predict(modelnb, univ_train)
a=table(univ_train$Loan, pred)
e_nbtr=(a[1,2]+a[2,1])/(a[1,1]+a[2,2]+a[1,2]+a[2,1])*100
e_nbtr
r_nbtr=(a[2,2])/(a[2,1]+a[2,2])*100
r_nbtr
table(univ_test$Loan, predict(modelnb, univ_test))
a=table(univ_test$Loan, predict(modelnb, univ_test))
r_nbte=(a[2,2])/(a[2,1]+a[2,2])*100
r_nbte
univs = univ[, c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 7)]
require(DMwR)
univs = SMOTE(Loan ~ ., univs, perc.over = 400)
rownames(univs)=as.character(seq(1:6240))
fix(univs)
table(univs$Loan)

train = sample(1:6280,4000) 
univs_train = univs[train,] 
test = (1:6280) [-train] 
univs_test = univs[test,] 


table(univs$Loan)
table(univs_train$Loan)
table(univs_test$Loan)

modelnbs = naiveBayes(Loan ~ ., data = univs_train)
table(univs_train$Loan, predict(modelnbs, univs_train))
a=table(univs_train$Loan, predict(modelnbs, univs_train))
r_nbstr=(a[2,2])/(a[2,1]+a[2,2])*100
r_nbstr
table(univs_test$Loan, predict(modelnbs, univs_test))
a=table(univs_test$Loan, predict(modelnbs, univs_test))
r_nbste=(a[2,2])/(a[2,1]+a[2,2])*100
r_nbste
table(univ_test$Loan, predict(modelnbs, univ_test))
a=table(univ_test$Loan, predict(modelnbs, univ_test))
r_nbsote=(a[2,2])/(a[2,1]+a[2,2])*100
r_nbsote
library(rpart)
modeldtcart <- rpart(Loan ~ ., data = univ_train, method="class", cp=0.001)
prettyTree(modeldtcart)
printcp(modeldtcart)

library(RWeka)
modeldtc45= J48(Loan ~ ., data = univ_train)
summary(modeldtc45)
table(univ_train$Loan, predict(modeldtc45))
a=table(univ_train$Loan, predict(modeldtc45, univ_train))
r_c45tr=(a[2,2])/(a[2,1]+a[2,2])*100
r_c45tr

table(univ_test$Loan, predict(modeldtc45, univ_test))
a=table(univ_test$Loan, predict(modeldtc45, univ_test))
r_c45te=(a[2,2])/(a[2,1]+a[2,2])*100
r_c45te

modelsdtc45= J48(Loan ~ ., data = univs_train)
table(univs_train$Loan, predict(modelsdtc45))
a=table(univs_train$Loan, predict(modelsdtc45, univs_train))
r_c45str=(a[2,2])/(a[2,1]+a[2,2])*100
r_c45str

table(univs_test$Loan, predict(modelsdtc45, univs_test))
a=table(univs_test$Loan, predict(modelsdtc45, univs_test))
r_c45ste=(a[2,2])/(a[2,1]+a[2,2])*100
r_c45ste

library(randomForest)
set.seed=1234
modelrf=randomForest(Loan~., data=univ_train, ntree=700)
plot(modelrf)
importance(modelrf)
table(univ_train$Loan, predict(modelrf))
a=table(univ_train$Loan, predict(modelrf, univ_train))
r_rftr=(a[2,2])/(a[2,1]+a[2,2])*100
r_rftr

table(univ_test$Loan, predict(modelrf, univ_test))
a=table(univ_test$Loan, predict(modelrf, univ_test))
r_rfte=(a[2,2])/(a[2,1]+a[2,2])*100
r_rfte

modelab <- AdaBoostM1(Loan ~ .,univ_train, control=Weka_control(I=100))
table(univ_train$Loan, predict(modelab))
a=table(univ_train$Loan, predict(modelab, univ_train))
r_abtr=(a[2,2])/(a[2,1]+a[2,2])*100

table(univ_test$Loan, predict(modelab, univ_test))
a=table(univ_test$Loan, predict(modelab, univ_test))
r_abte=(a[2,2])/(a[2,1]+a[2,2])*100

recall_comp=matrix(c(r_nbtr, r_nbte, r_nbstr, r_nbste, r_c45tr, r_c45te, r_rftr, r_rfte, r_abtr, r_abte), nrow=1) 
colnames(recall_comp)=c('NB_Tr', 'NB_TE', 'NBS_TR', 'NBS_TE', 'C4.5TR', 'C4.5TE', 'RFTR', 'RFTE', 'ABTR', 'ABTE')
recall_comp
