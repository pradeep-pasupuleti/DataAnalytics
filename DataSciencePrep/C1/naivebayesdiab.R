
library(caret)

library(e1071)

setwd("C:/Pradeep/Working Set/Consulting/Kaggle/DataSciencePrep/InsofeChap1/Data")
diabdata <- read.csv("pima-indians-diabetes_data.csv")
dim(diabdata)
summary(diabdata)
names(diabdata)
str(diabdata)
dim(diabdata)
attributes(diabdata)
is.na(diabdata)
head(diabdata,n=10)
diabdataNA= na.omit(diabdata)


# partitioning the df into 70% 30%
set.seed(3456)
trainIndex <- createDataPartition(diabdataNA$class, p = .7,list = FALSE,times = 1)
head(trainIndex)

diabTrain <- diabdataNA[ trainIndex,]
diabTest  <- diabdataNA[-trainIndex,]
dim(diabTrain)
dim(diabTest)

# list rows of data that have missing values 
nrow(diabdata[!complete.cases(diabdata),])

# Calculating the data summary for the total 

print(" Calculating the data summary for the total data")
table(diabdataNA$class)

print(" Calculating the data summary for the Training data")
table(diabTrain$class)

print(" Calculating the data summary for the Testing data")
table(diabTest$class)

print(" Building the naivebayes model  for the Training data")
nbmodel = naiveBayes(class ~ ., data = diabTrain, na.action =  na.omit)
#nbmodel

print(" Using the naivebayes model  on the testing data to perform the predictions")
pred = predict(nbmodel, diabdata)
length(pred)
length(diabTest)
nrow(diabTest)
table(pred, diabTest$class)

print(" Using the naivebayes model  on the training data to perform the predictions")
pred = predict(nbmodel, diabTrain)
pred
table(pred, diabTrain$class)

