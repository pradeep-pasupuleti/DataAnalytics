library(caret)
library(e1071)
library(rattle)
data(HouseVotes84)
??HouseVotes84
house <-(HouseVotes84)
nahouse <- na.omit(HouseVotes84)
nrow(house)
nrow(nahouse)
str(house)
head(house,n=10)
# list rows of data that have missing values 
nrow(diabdata[!complete.cases(house),])

# partitioning the df into 70% 30%

set.seed(344)
trainidx <- createDataPartition(nahouse$Class, p = .7,list = FALSE,times = 1)
trainidx

houseTrain <- nahouse[ trainidx,]
houseTest  <- nahouse[-trainidx,]
dim(nahouse)
dim(houseTrain)
dim(houseTest)

length(nahouse)
length(houseTrain)
length(houseTest)


print(" Calculating the data summary for the total data")
table(nahouse$Class)

print(" Calculating the data summary for the Training data")
table(houseTrain$Class)

print(" Calculating the data summary for the Testing data")
table(houseTest$Class)

print(" Building the naivebayes model  for the Training data")
homodel = naiveBayes(Class ~ ., data = houseTrain, na.action =  na.omit)
#homodel

print(" Using the naivebayes model  on the testing data to perform the predictions")
hopred = predict(homodel, houseTest)
length(hopred)
length(houseTest)

table(hopred, houseTest$Class)

print(" Using the naivebayes model  on the training data to perform the predictions")
hopred = predict(homodel, houseTrain)
length(hopred)
length(houseTrain)
table(hopred, houseTrain$class)

rattle()
??FSelector
