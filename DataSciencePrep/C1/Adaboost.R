setwd("C:/Pradeep/Working Set/Consulting/Kaggle/DataAnalytics/DataSciencePrep/C1/Data")
bankdata = read.table('UniversalBank.csv', header=T, sep=',')

bankdata2=subset(bankdata, select=-c(ID,ZIP.Code)) # to remove the columns ID & ZIP Code from
fix(bankdata2)
summary(bankdata2)
library(dummies)
Education=dummy(bankdata2$Education)

bankdata3=subset(bankdata2,select=-c(Education))
bankdata4=cbind(bankdata3,Education)

library(vegan)
bankdata5=decostand(bankdata4,"range")
fix(bankdata5)

train = sample(1:5000,3000) # to take a random sample of 60% of the records for train data
train_data = bankdata5[train,]
nrow(train_data)

test = (1:5000) [-train] # to take a random sample of 40% of the records for test data
test_data = bankdata5[test,]
nrow(test_data)
# added some comments

#Total Data
table(bankdata5$Personal.Loan)
#Train Data
table(train_data $Personal.Loan)
#Test Data
table(test_data$Personal.Loan)

library(ada)
#Install & Load the package ― ada to perform SVM analysis.
x = subset(train_data, select = -Personal.Loan)
y = as.factor(train_data$Personal.Loan)
gdis=ada(x,y,iter=20,nu=1,loss="logistic", type="discrete") #20 iterations of boosting
summary(gdis)
gdis
a = subset(test_data, select = -Personal.Loan)
b = as.factor(test_data$Personal.Loan)

gdis=addtest(gdis,a,b)
##add testing data set
gdis
pred = predict(gdis, a)
table(pred, b)
plot(gdis,TRUE,TRUE)

gdis=ada(x,y,iter=100,nu=1,loss="logistic", type="discrete")
gdis=addtest(gdis,a,b)
pred = predict(gdis, a)
table(pred, b)
plot(gdis,TRUE,TRUE)
