# Build a simple Naive Bayes model

# Upload the required library
library(e1071)
library(pmml)
library(mlbench)

# download an example dataset
data(HouseVotes84)
house <- na.omit(HouseVotes84)

# Construct an example model defining a threshold value of 0.003
model<-naiveBayes(Class~V1+V2+V3,data=house,threshold=0.003)

# Output the PMML representation 
pmml(model,dataset=house,predictedField="Class")
