closeAllConnections()
rm(list=ls())

readData <- function(path.name, file.name, column.types, missing.types) {
  read.csv( url( paste(path.name, file.name, sep="") ), 
            colClasses=column.types,
            na.strings=missing.types )
}

Titanic.path <- "https://raw.github.com/wehrley/Kaggle_Titanic/master/"
train.data.file <- "train.csv"
test.data.file <- "test.csv"
missing.types <- c("NA", "")
train.column.types <- c('integer',   # PassengerId
                        'factor',    # Survived 
                        'factor',    # Pclass
                        'character', # Name
                        'factor',    # Sex
                        'numeric',   # Age
                        'integer',   # SibSp
                        'integer',   # Parch
                        'character', # Ticket
                        'numeric',   # Fare
                        'character', # Cabin
                        'factor'     # Embarked
)
test.column.types <- train.column.types[-2]     # # no Survived column in test.csv

train.raw <- readData(Titanic.path, train.data.file, 
                      train.column.types, missing.types)
df.train <- train.raw

test.raw <- readData(Titanic.path, test.data.file, 
                     test.column.types, missing.types)
df.infer <- test.raw   

## map missing data by provided feature
require(Amelia)
missmap(df.train,  main="Titanic Training Data - Missings Map", 
        col=c("wheat", "darkred"), legend=FALSE)

barplot(table(df.train$Survived),
        names.arg = c("Perished", "Survived"),
        main="Survived (passenger fate)", col="black")
barplot(table(df.train$Pclass), 
        names.arg = c("first", "second", "third"),
        main="Pclass (passenger traveling class)", col="firebrick")
barplot(table(df.train$Sex), main="Sex (gender)", col="darkviolet")
hist(df.train$Age, main="Age", xlab = NULL, col="brown")
barplot(table(df.train$SibSp), main="SibSp (siblings + spouse aboard)", 
        col="darkblue")
barplot(table(df.train$Parch), main="Parch (parents + kids aboard)", 
        col="gray50")
hist(df.train$Fare, main="Fare (fee paid for ticket[s])", xlab = NULL, 
     col="darkgreen")
barplot(table(df.train$Embarked), 
        names.arg = c("Cherbourg", "Queenstown", "Southampton"),
        main="Embarked (port of embarkation)", col="sienna")

boxplot(df.train$Age ~ df.train$Survived, 
        main="Passenger Fate by Age",
        xlab="Survived", ylab="Age")

boxplot(df.train$Age ~ df.train$Pclass, 
        main="Passenger travelling class by Age",
        xlab="Survived", ylab="Class")
# Mosaic plots offer an interesting -- and arguably under-utilized -- way to summarize data.
require(vcd)
mosaicplot(df.train$Pclass ~ df.train$Survived, 
           main="Passenger Fate by Traveling Class", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

mosaicplot(df.train$Sex ~ df.train$Survived, 
           main="Passenger Fate by Sex", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

mosaicplot(df.train$Embarked ~ df.train$Survived, 
           main="Passenger Fate by Embarking station", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

mosaicplot(df.train$Fare ~ df.train$Survived, 
           main="Passenger Fate by Embarking station", shade=FALSE, 
           color=TRUE, xlab="Pclass", ylab="Survived")

require(corrgram)
corrgram.data <- df.train
## change features of factor type to numeric type for inclusion on correlogram
corrgram.data$Survived <- as.numeric(corrgram.data$Survived)
corrgram.data$Pclass <- as.numeric(corrgram.data$Pclass)
corrgram.data$Embarked <- revalue(corrgram.data$Embarked, 
                                  c("C" = 1, "Q" = 2, "S" = 3))
## generate correlogram
corrgram.vars <- c("Survived", "Pclass", "Sex", "Age", 
                   "SibSp", "Parch", "Fare", "Embarked")
corrgram(corrgram.data[,corrgram.vars], order=FALSE, 
         lower.panel=panel.ellipse, upper.panel=panel.pie, 
         text.panel=panel.txt, main="Titanic Training Data")

## function for extracting honorific (i.e. title) from the Name feature
getTitle <- function(data) {
  title.dot.start <- regexpr("\\,[A-Z ]{1,20}\\.", data$Name, TRUE)
  title.comma.end <- title.dot.start  + attr(title.dot.start, "match.length")-1
  data$Title <- substr(data$Name, title.dot.start+2, title.comma.end-1)
  return (data$Title)
}   

df.train$Title <- getTitle(df.train)
unique(df.train$Title)

# To identify the titles which have at least one record with an age missing,
# I'll use the bystats function from the Hmisc package.
options(digits=2)
library(Hmisc)
bystats(df.train$Age, df.train$Title, 
        fun=function(x)c(Mean=mean(x),Median=median(x)))

imputeMedian <- function(impute.var, filter.var, var.levels) {
  for (v in var.levels) {
    impute.var[ which( filter.var == v)] <- impute(impute.var[ 
      which( filter.var == v)])
  }
  return (impute.var)
}

## list of titles with missing Age value(s) requiring imputation
titles.na.train <- c("Dr", "Master", "Mrs", "Miss", "Mr")
df.train$Age[which(df.train$Title=="Dr")]

df.train$Age <- imputeMedian(df.train$Age, df.train$Title, 
                            titles.na.train)

summary(df.train$Embarked)

# t should be fine to replace those missings with "S", the most common value.

df.train$Embarked[which(is.na(df.train$Embarked))] <- 'S'

# While there are no missing Fare values, a summary does show at least one Fare=0...

summary(df.train$Fare)

subset(df.train, Fare < 7)[order(subset(df.train, Fare < 7)$Fare, 
                                 subset(df.train, Fare < 7)$Pclass), 
                           c("Age", "Title", "Pclass", "Fare")]

# I replaced the zero Fare values with the median fare from the respective
# passenger class using the imputMedian function introduced earlier.

## impute missings on Fare feature with median fare by Pclass
df.train$Fare[ which( df.train$Fare == 0 )] <- NA
df.train$Fare <- imputeMedian(df.train$Fare, df.train$Pclass, 
                              as.numeric(levels(df.train$Pclass)))

df.train$Title <- factor(df.train$Title,
                         c("Capt","Col","Major","Sir","Lady","Rev",
                           "Dr","Don","Jonkheer","the Countess","Mrs",
                           "Ms","Mr","Mme","Mlle","Miss","Master"))
boxplot(df.train$Age ~ df.train$Title, 
        main="Passenger Age by Title", xlab="Title", ylab="Age")

## function for assigning a new title value to old title(s) 
changeTitles <- function(data, old.titles, new.title) {
  for (honorific in old.titles) {
    data$Title[ which( data$Title == honorific)] <- new.title
  }
  return (data$Title)
}
## Title consolidation
df.train$Title <- changeTitles(df.train, 
                               c("Capt", "Col", "Don", "Dr", 
                                 "Jonkheer", "Lady", "Major", 
                                 "Rev", "Sir"),"Noble")
df.train$Title <- changeTitles(df.train, c("the Countess", "Ms"), 
                               "Mrs")
df.train$Title <- changeTitles(df.train, c("Mlle", "Mme"), "Miss")
df.train$Title <- as.factor(df.train$Title)

library(plyr)     # for the revalue function 
library(stringr)  # for the str_sub function

## test a character as an EVEN single digit
isEven <- function(x) x %in% c("0","2","4","6","8") 
## test a character as an ODD single digit
isOdd <- function(x) x %in% c("1","3","5","7","9") 

## function to add features to training or test data frames
featureEngrg <- function(data) {
  ## Using Fate ILO Survived because term is shorter and just sounds good
  data$Fate <- data$Survived
  ## Revaluing Fate factor to ease assessment of confusion matrices later
  data$Fate <- revalue(data$Fate, c("1" = "Survived", "0" = "Perished"))
  ## Boat.dibs attempts to capture the "women and children first"
  ## policy in one feature.  Assuming all females plus males under 15
  ## got "dibs' on access to a lifeboat
  data$Boat.dibs <- "No"
  data$Boat.dibs[which(data$Sex == "female" | data$Age < 15)] <- "Yes"
  data$Boat.dibs <- as.factor(data$Boat.dibs)
  ## Family consolidates siblings and spouses (SibSp) plus
  ## parents and children (Parch) into one feature
  data$Family <- data$SibSp + data$Parch
  ## Fare.pp attempts to adjust group purchases by size of family
  data$Fare.pp <- data$Fare/(data$Family + 1)
  ## Giving the traveling class feature a new look
  data$Class <- data$Pclass
  data$Class <- revalue(data$Class, 
                        c("1"="First", "2"="Second", "3"="Third"))
  ## First character in Cabin number represents the Deck 
  data$Deck <- substring(data$Cabin, 1, 1)
  data$Deck[ which( is.na(data$Deck ))] <- "UNK"
  data$Deck <- as.factor(data$Deck)
  ## Odd-numbered cabins were reportedly on the port side of the ship
  ## Even-numbered cabins assigned Side="starboard"
  data$cabin.last.digit <- str_sub(data$Cabin, -1)
  data$Side <- "UNK"
  data$Side[which(isEven(data$cabin.last.digit))] <- "port"
  data$Side[which(isOdd(data$cabin.last.digit))] <- "starboard"
  data$Side <- as.factor(data$Side)
  data$cabin.last.digit <- NULL
  return (data)
}

## add remaining features to training data frame
df.train <- featureEngrg(df.train)

# finish the data munging process by paring down the data frame to the columns I will use in model building.

train.keeps <- c("Fate", "Sex", "Boat.dibs", "Age", "Title", 
                 "Class", "Deck", "Side", "Fare", "Fare.pp", 
                 "Embarked", "Family")
df.train.munged <- df.train[train.keeps]

require(caret)
## split training data into train batch and test batch
set.seed(23)
nRowEmps<-nrow(df.train.munged)
nTrnSampPercent = nRowEmps*0.8 # we can change this 
ntrainrows = sample(1:nRowEmps,nTrnSampPercent) # to take a random sample of 60% of the records for train

train.batch = df.train.munged[ntrainrows,]
dim(train.batch) # Sanity check

nvalidaterows = (1:nRowEmps) [-ntrainrows] # to take a random sample of 40% of the records for test data
test.batch = df.train.munged[nvalidaterows,]
dim(test.batch)

# training.rows <- createDataPartition(df.train.munged$Survived, p = 0.8, list = FALSE)
# train.batch <- df.train.munged[training.rows, ]
# test.batch <- df.train.munged[-training.rows, ]

Titanic.logit.1 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare, data = train.batch, family=binomial("logit"))

summary(Titanic.logit.1)
anova(Titanic.logit.1, test="Chisq")

# Notice how the Sex and Class features accounted for the lion's share of the
# reduction in the deviance, providing some support to our hypotheses about life
# boat access and location on ship. Since Fare isn't doing much for us, let's
# see if the Fare.pp we created fares any better (pun intended).

Titanic.logit.2 <- glm(Fate ~ Sex + Class + Age + Family + Embarked + Fare.pp,data = train.batch, family=binomial("logit"))
anova(Titanic.logit.2, test="Chisq")


# Hmm, that was no help. Dropping fares altogether and passing a slightly
# slimmer formula through the glm() function will give us a new baseline for
# model improvement.

Titanic.logit.3 <-glm(Fate ~ Sex + Class + Age + Family + Embarked, data = train.batch, family=binomial("logit"))
anova(Titanic.logit.3, test="Chisq")

## Define control function to handle optional arguments for train function
## Models to be assessed based on largest absolute area under ROC curve
cv.ctrl <- trainControl(method = "repeatedcv", repeats = 3,
                        summaryFunction = twoClassSummary,
                        classProbs = TRUE)

# Below is the train function call using the same formula (sans Fare) that we
# recently passed through glm function. I use the metric argument to tell train
# to optimize the model by maximizing the area under the ROC curve (AUC).
# summary(), another extractor function, is called to generate regression
# coefficients with standard errors and a z-test, plus the residual deviance
# metric we were watching earlier.

set.seed(35)
glm.tune.1 <- train(Fate ~ Sex + Class + Age + Family + Embarked,
                    data = train.batch,
                    method = "glm",
                    metric = "ROC",
                    trControl = cv.ctrl)
glm.tune.1
summary(glm.tune.1)

set.seed(35)
glm.tune.5 <- train(Fate ~ Class + I(Title=="Mr") + I(Title=="Noble") 
                      + Age + Family + I(Embarked=="S") 
                      + I(Title=="Mr"&Class=="Third"), 
                      data = train.batch, 
                      method = "glm", metric = "ROC", 
                      trControl = cv.ctrl)
summary(glm.tune.5)

# First up is boosting. I can instruct train to fit a stochastic boosting model
# for the binary response Fate using the adapackage and a range of values for
# each of three tuning parameters. Concretely, when fitting a model using train
# with method=”ada”, one has three levers to tweak: iter (number of boosting
# iterations, default=50), maxdepth (depth of trees), and nu (shrinkage
# parameter, default=1).

# note the dot preceding each variable
ada.grid <- expand.grid(.iter = c(50, 100),
                        .maxdepth = c(4, 8),
                        .nu = c(0.1, 1))

set.seed(35)
ada.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                  data = train.batch,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = ada.grid,
                  trControl = cv.ctrl)

ada.tune
plot(ada.tune) 

# Time to give the popular Random Forest (RF) model a shot at the Titanic
# challenge. The number of randomly pre-selected predictor variables for each
# node, designated mtry, is the sole parameter available for tuning an RF with
# train. Since the number of features is so small, there really isn't much scope
# for tuning mtry in this case. Nevertheless, I'll demonstrate here how it can
# be done. Let's have mtry=2 and mtry=3 duke it out over the Titanic data.

rf.grid <- data.frame(.mtry = c(2, 3))
set.seed(35)
rf.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                  data = train.batch,
                  method = "rf",
                  metric = "ROC",
                  tuneGrid = rf.grid,
                  trControl = cv.ctrl)

# Strobl et al suggested setting mtry at the square root of the number of
# variables. In this case, that would be mtry = 2, which did produce the better
# RF model.

rf.tune

# And finally, we'll fit a support vector machine (SVM) model to the Titanic
# data. There are two functions which can be tuned for SVM using train. The
# default value for one of them -– sigest –- produces good results on most
# occasions. The default grid of cost parameter C is 0.25, 0.5, and 1. If we set
# train argument tuneLength = 9, the grid expands to c(0.25, 0.5, 1, 2, 4, 8,
# 16, 32, 64). As SVM is considered sensitive to the scale and magnitude of the
# presented features, I'll use the preProcess argument to instruct train to make
# arrangements for normalizing the data within resampling loops.

set.seed(35)
svm.tune <- train(Fate ~ Sex + Class + Age + Family + Embarked, 
                  data = train.batch,
                  method = "svmRadial",
                  tuneLength = 9,
                  preProcess = c("center", "scale"),
                  metric = "ROC",
                  trControl = cv.ctrl)
svm.tune

# Model Evaluation
## Logistic regression model
glm.pred <- predict(glm.tune.5, test.batch)
confusionMatrix(glm.pred, test.batch$Fate)

## Boosted model
ada.pred <- predict(ada.tune, test.batch)
confusionMatrix(ada.pred, test.batch$Fate)


## Random Forest model
rf.pred <- predict(rf.tune, test.batch)
confusionMatrix(rf.pred, test.batch$Fate)

#SVM
svm.pred <- predict(svm.tune, test.batch)
confusionMatrix(svm.pred, test.batch$Fate)

require(pROC)
## Logistic regression model (BLACK curve)
glm.probs <- predict(glm.tune.5, test.batch, type = "prob")
glm.ROC <- roc(response = test.batch$Fate,
               predictor = glm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(glm.ROC, type="S")   
## Area under the curve: 0.8609 
## Boosted model (GREEN curve)
ada.probs <- predict(ada.tune, test.batch, type = "prob")
ada.ROC <- roc(response = test.batch$Fate,
               predictor = ada.probs$Survived,
               levels = levels(test.batch$Fate))
plot(ada.ROC, add=TRUE, col="green")    
## Area under the curve: 0.8759
## Random Forest model (RED curve)
rf.probs <- predict(rf.tune, test.batch, type = "prob")
rf.ROC <- roc(response = test.batch$Fate,
              predictor = rf.probs$Survived,
              levels = levels(test.batch$Fate))
plot(rf.ROC, add=TRUE, col="red") 
## Area under the curve: 0.8713
## SVM model (BLUE curve)
svm.probs <- predict(svm.tune, test.batch, type = "prob")
svm.ROC <- roc(response = test.batch$Fate,
               predictor = svm.probs$Survived,
               levels = levels(test.batch$Fate))
plot(svm.ROC, add=TRUE, col="blue")
## Area under the curve: 0.8077

# The following R script uses caret function resamples to collect the resampling
# results, then calls the dotplot function to create a visualization of the
# resampling distributions. I'm typically not one for leaning on a single metric
# for important decisions, but if you have been looking for that one graph which
# sums up the performance of the four models, this is it.

library(caret)

cv.values <- resamples(list(Logit = glm.tune.5, Ada = ada.tune, RF = rf.tune, SVM = svm.tune))
dotplot(cv.values, metric = "ROC")


# Given everything we've been through here, it would be a shame if we didn't
# submit at least one of the four models to the Titanic competition at Kaggle.
# Here is a script which munges the data Kaggle provided in their test.csv file,
# uses that data and the logistic regression model glm.tune.5 to predict the
# survival (or not) of passengers listed in the test file, links the predictions
# to the PassengerId in a data frame, and writes those results to a
# submission-ready csv file.

# get titles
df.infer$Title <- getTitle(df.infer)

# impute missing Age values
df.infer$Title <- changeTitles(df.infer, c("Dona", "Ms"), "Mrs")
titles.na.test <- c("Master", "Mrs", "Miss", "Mr")
df.infer$Age <- imputeMedian(df.infer$Age, df.infer$Title, titles.na.test)

# consolidate titles
df.infer$Title <- changeTitles(df.infer, c("Col", "Dr", "Rev"), "Noble")
df.infer$Title <- changeTitles(df.infer, c("Mlle", "Mme"), "Miss")
df.infer$Title <- as.factor(df.infer$Title)

# impute missing fares
df.infer$Fare[ which( df.infer$Fare == 0)] <- NA
df.infer$Fare <- imputeMedian(df.infer$Fare, df.infer$Pclass, 
                              as.numeric(levels(df.infer$Pclass)))
# add the other features
df.infer <- featureEngrg(df.infer)

# data prepped for casting predictions
test.keeps <- train.keeps[-1]
pred.these <- df.infer[test.keeps]

# use the logistic regression model to generate predictions
Survived <- predict(glm.tune.5, newdata = pred.these)

# reformat predictions to 0 or 1 and link to PassengerId in a data frame
Survived <- revalue(Survived, c("Survived" = 1, "Perished" = 0))
predictions <- as.data.frame(Survived)
predictions$PassengerId <- df.infer$PassengerId

# write predictions to csv file for submission to Kaggle
write.csv(predictions[,c("PassengerId", "Survived")], 
          file="Titanic_predictions.csv", row.names=FALSE, quote=FALSE)