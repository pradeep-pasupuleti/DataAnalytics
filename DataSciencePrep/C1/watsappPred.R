## Import the Data scraped from  blog using Import.IO

## wtsp is the Final Generated Data Set

##Import All Libraries
library(tm)
library(SnowballC)
closeAllConnections()
rm(list=ls())


wtsp<-read.table("DataSciencePrep/C1/Data/watsapp1.csv",sep=",",stringsAsFactors=FALSE,header=T,quote = "\"",allowEscapes=TRUE,fill=TRUE)

# added a comment
##Select column from where Text needs to be Extracted
# pos.time<-character(0)
pos.text<-character(0)
# pos.Date<-character(0)
# pos.Name<-character(0)
# 
# pos.time<-wtsp$Time
pos.text<-wtsp$Text
# pos.Date<-wtsp$Date
# pos.Name<-wtsp$Name


##Create Corpus as reqd by TM package
##Corpus requires a vector source
# pos.corpus.time<-Corpus(VectorSource(pos.time))
pos.corpus.text<-Corpus(VectorSource(pos.text))
# pos.corpus.date<-Corpus(VectorSource(pos.Date))
# pos.corpus.name<-Corpus(VectorSource(pos.Name))


# inspect(pos.corpus.time) ##Similarly other corpuses can be inspected
# inspect(pos.corpus.text)
# inspect(pos.corpus.date)
# inspect(pos.corpus.name)

##Lets Do some pre processing
##Removing words from Comments which have higher correlation to Output Classification
ExtraStopWords<-c(stopwords("english"), unique(wtsp$Name))

Pos.corpus.Preprocess=function(corpus)
{
  corpus<-tm_map(corpus,stripWhitespace) ##Remove extra white spaces
  corpus<-tm_map(corpus,removePunctuation)  ## Remove Punctuations
  corpus<-tm_map(corpus,removeNumbers)  ## Remove Numbers
  corpus<-tm_map(corpus,removeWords,stopwords("english"))   ## Remove Stop Words
  corpus<-tm_map(corpus,tolower) ## Converts to Lower case
  corpus<-tm_map(corpus, stemDocument, language = "english")
  corpus <- tm_map(corpus, PlainTextDocument)
  return(corpus)
}

# pos.corpus.time<-Pos.corpus.Preprocess(pos.corpus.time)
pos.corpus.text<-Pos.corpus.Preprocess(pos.corpus.text)
# pos.corpus.date<-Pos.corpus.Preprocess(pos.corpus.date)
# pos.corpus.name<-Pos.corpus.Preprocess(pos.corpus.name)


##Generate a Document Term Matrix, Train Dataset and Test Dataset
##Input the sparse for Title, Test, Tags, Comments and Categories accordingly such that Accuracy can be enhanced 

# Pos.DTM<-function(sparseTitle,sparseText,sparseTags,sparseComments,sparseCategories)
# {
#   pos.DTM.time<-removeSparseTerms(DocumentTermMatrix(pos.corpus.time),sparseTitle)
#   (pos.DTM.time)
#   pos.DTM.text<-removeSparseTerms(DocumentTermMatrix(pos.corpus.text),sparseText)
#   (pos.DTM.text)
#   pos.DTM.date<-removeSparseTerms(DocumentTermMatrix(pos.corpus.date),sparseTags)
#   (pos.DTM.date)
#   pos.DTM.name<-removeSparseTerms(DocumentTermMatrix(pos.corpus.name),sparseComments)
#   (pos.DTM.name)
#   
# }
# pos.DTM.time<-removeSparseTerms(DocumentTermMatrix(pos.corpus.time),.99)

pos.DTM.text<-removeSparseTerms(TermDocumentMatrix(pos.corpus.text),0.99)
pos.DTM.text<-DocumentTermMatrix(pos.corpus.text)
pos.DTM.text.Sparse<-removeSparseTerms(pos.DTM.text,0.99)
# pos.DTM.date<-removeSparseTerms(DocumentTermMatrix(pos.corpus.date),0.99)
# 
# pos.DTM.name<-removeSparseTerms(DocumentTermMatrix(pos.corpus.name),0.99)

findFreqTerms(pos.DTM.text, lowfreq=10)
findFreqTerms(pos.DTM.text.Sparse, lowfreq=30)

termFrequency <- colSums(as.matrix(pos.DTM.text))
termFrequency <- subset(termFrequency, termFrequency>=10)
library(ggplot2)
qplot(names(termFrequency), termFrequency, stat="identity",geom="bar", xlab="Terms") + coord_flip()

library(wordcloud)
m <- as.matrix(pos.DTM.text)
# calculate the frequency of words and sort it descendingly by frequency
wordFreq <- sort(colSums(m), decreasing=TRUE)
# word cloud
set.seed(375) # to make it reproducible
grayLevels <- blue( (wordFreq+10) / (max(wordFreq)+10) )
wordcloud(words=names(wordFreq), freq=wordFreq, min.freq=30,random.color=T, random.order=F,colors=grayLevels)

# Finding associate
good_assoc<-findAssocs(pos.DTM.text, 'good', 0.0) 
length(good_assoc)
pradeep_assoc<-findAssocs(pos.DTM.text, 'pradeep', 0.0) 
length(pradeep_assoc)
srikanth_assoc<-findAssocs(pos.DTM.text, 'srikanth', 0.0) 
length(srikanth_assoc)

# Function to create cluster
fndrawClust<-function(mydata.dtm, strMethod, nClusters){
  # convert the sparse term-document matrix to a standard data frame
  mydata.df <- as.data.frame(inspect(mydata.dtm))
  mydata.df.scale <- scale(mydata.df)
  dim(mydata.df.scale)
  d <- dist(mydata.df.scale, method = strMethod) # distance matrix
  fit <- hclust(d, method="ward.D")
  plot(fit) # display dendogram?
  
  groups <- cutree(fit, k=nClusters) # cut tree into 5 clusters
  # draw dendogram with red borders around the 5 clusters
  rect.hclust(fit, k=nClusters, border="red")
}

fndrawClust(pos.DTM.text.Sparse,"euclidean",10)

fndrawClust(pos.DTM.text.Sparse,"minkowski",5)

# Kmeans clustering

# transpose the matrix to cluster documents 
fndrawKMClust<-function(mydata.dtm, k){
  mydata.df <- as.data.frame(inspect(mydata.dtm))
  m3 <- t(mydata.df)
  # set a fixed random seed
  set.seed(122)
  # k-means clustering of tweets
  
  kmeansResult <- kmeans(m3, k)
  # cluster centers
  round(kmeansResult$centers, digits=3)
}
kmeansResult<-fndrawKMClust(pos.DTM.text.Sparse,8)
fndrawKMClust(pos.DTM.text,3)

# for (i in 1:8) {
#    cat(paste("cluster ", i, ": ", sep=""))
#    s <- sort(kmeansResult$centers[i,], decreasing=T)
#    cat(names(s)[1:3], "\n")
#    # print the tweets of every cluster
#     
#    # print(rdmTweets[which(kmeansResult$cluster==i)])
#      }

fnSNA<-function(termDocMatrix){
  
  # change it to a Boolean matrix
  #termDocMatrix[termDocMatrix>=1] <- 1
  # transform into a term-term adjacency matrix
  #   In the above code, %*% is an operator for the product of two matrices, and
  #   t() transposes a matrix. Now we have built a term-term adjacency matrix,
  #   where the rows and columns represent terms, and every entry is the number of
  #   concurrences of two terms.
  termDocMatrix<-as.matrix(termDocMatrix)
  termMatrix <- termDocMatrix %*% t(termDocMatrix)
  # inspect terms numbered 5 to 10
  termMatrix[5:10,5:10]
  
  library(igraph)
  # build a graph from the above matrix
  g <- graph.adjacency(termMatrix, weighted=T, mode="undirected")
  # remove loops
  g <- simplify(g)
  # set labels and degrees of vertices
  V(g)$label <- V(g)$name
  V(g)$degree <- degree(g)
  # set seed to make the layout reproducible
  set.seed(3952)
  layout1 <- layout.fruchterman.reingold(g)
  plot(g, layout=layout1)
}
debug(fnSNA)
fnSNA(pos.DTM.text.Sparse)
undebug(fnSNA)
##Now Generate Document Term Matrix
# Final_DTM<-cbind(inspect(pos.DTM.time),inspect(pos.DTM.text),inspect(pos.DTM.date),inspect(pos.DTM.name))
Final_DTM<-cbind(inspect(pos.DTM.text))
dim(Final_DTM)

names(Final_DTM)
wtsp_Rem<-wtsp[,-c(1,2,4)]
wtsp_matrix<-as.matrix(wtsp_Rem)

Final_wtsp<-cbind(Final_DTM,wtsp_matrix)  ## Matrix form
Final_wtsp_Df<-as.data.frame(Final_wtsp)   ##DataFrame form
sort(names(Final_wtsp_Df))
#Renaming the col
names(Final_wtsp_Df)[names(Final_wtsp_Df)=="V74"] <- "Name"
##Upon Running the above as.data.frame, The parameters such as Month, Date and Year transforms themselves into Factors
##Converting them back to Numeric
# Final_wtsp_Df$Month<-as.numeric(levels(Final_wtsp_Df$Month)[Final_wtsp_Df$Month])
# Final_wtsp_Df$Date<-as.numeric(levels(Final_wtsp_Df$Date)[Final_wtsp_Df$Date])
# Final_wtsp_Df$Year<-as.numeric(levels(Final_wtsp_Df$Year)[Final_wtsp_Df$Year])
# ##Ensuring Date Format
# Final_wtsp_Df$Date<-as.Date(as.character(Final_wtsp_Df$Date),format="%d/%m/%Y")
# head(Final_wtsp_Df$Date)
# return(Final_wtsp_Df)
# }

##Generate Final Dataframe for Analysis. Sprase will change with every model for better accuracy
#Final_wtsp_Df<-Pos.DTM(0.8,0.5,0.4,0.5,0.8)

## Partition the Data into Train and Test Dataset

Train_wtsp<-subset(Final_wtsp_Df,Final_wtsp_Df$CompDate<="2014-07-06")
Test_wtsp<-subset(Final_wtsp_Df,Final_wtsp_Df$CompDate>"2014-07-06")

nRowEmps<-nrow(Final_wtsp_Df)
nTrnSampPercent = nRowEmps*0.6 # we can change this 
ntrainrows = sample(1:nRowEmps,nTrnSampPercent) # to take a random sample of 60% of the records for train

Train_wtsp = Final_wtsp_Df[ntrainrows,]
dim(Train_wtsp) # Sanity check

nvalidaterows = (1:nRowEmps) [-ntrainrows] # to take a random sample of 40% of the records for test data
Test_wtsp = Final_wtsp_Df[nvalidaterows,]
dim(Test_wtsp)


##Lets Start Classification Modelling

######################## 1. C5.0 Decision Tree   ################################

library(C50)

## Control for C5.0 Model
C5.0Control(subset=TRUE,bands=2,winnow=TRUE,noGlobalPruning=FALSE,CF=0.25,minCases=2,label="C5.0 Outcome")## Train Model using C5.0

##Train the Data
train.c50=C5.0(Train_wtsp[,-which(colnames(Test_wtsp)=="Author")],Train_wtsp$Author,trials=10,rules=FALSE,control=C5.0Control(),costs=NULL)
## Variable Importance Measure
C5imp(train.c50,metric="usage",pct=TRUE)## Lower % Usage variables can be dicarded and the Train function is re-run to get better accuracy
summary(train.c50) ## Check the percentage of accuracy of the model## Predict on the TEST data using trained Model
##Retraining Using higher Overall % Variables
Retrain.c50=C5.0(Train_wtsp[,c("articl","googl","note","number")],Train_wtsp$Author,trials=10,rules=FALSE,control=C5.0Control(),costs=NULL)

##Prediction on Test Data and Train Data
testPred.c50=predict(object=Retrain.c50,newdata=Test_wtsp[,c("articl","googl","note","number")],trials=1,type="class")
trainPred.c50<-predict(Retrain.c50,Train_wtsp[,c("articl","googl","note","number")],trials=1,type="class")

##Misclassification Matrix
MisClassTest<-table("Predict"=test.c50,"Actual"=Test_wtsp$Author)  ## Test Data Prediction
MisClassTrain<-table("Predict"=trainPred.c50,"Actual"=Train_wtsp$Author)   ## Train Data Prediction

##Accuracy based on Acceptance criteria
accuracyC50<-(100-mean(c((nrow(Test_wtsp)-sum(diag(MisClassTest)))/nrow(Test_wtsp)),(nrow(Train_wtsp)-sum(diag(MisClassTrain)))/nrow(Train_wtsp)))
accuracyC50


######################## 2. CART Model   ################################


library(rpart)
## Various parameters that control resursive partioning and regressive trees
control<-rpart.control(minsplit=30,minbucket=10,cp=0.01,maxcomplete=6,maxsurrogate=8,usersurrogate=2,xval=15,surrogatestyle=0,maxdepth=15)
## generally minimum no. of observations in any terminal leaf node(minbucket)=minimum no. of observations that must exist in a node in order to split (minsplit)/3

##Training the model
train.rpart<-rpart(Name~.,data=Train_wtsp,control=control)

##Prints the rpart object
print(train.rpart) 

##Summarizes the model
summary(train.rpart,digits=getOption("digits"),cp=0,save="train.rpart")

##Plot the rpart object
plot(train.rpart,uniform=FALSE,branch=1,compress=TRUE,margin=0)

##Label the plot drawn above (TREE Dendogram)
text(train.rpart,splits=TRUE,use.n=TRUE)

##PostScript Presentation Plot
post(train.rpart,file="",title="Analytics Vidya Text mining-RPART",use.n=TRUE)

##Snip-Contains the nodes that remain after selected subtrees are snipped off
train.rpart.snip<-snip.rpart(train.rpart,toss=5)  ## trim subtree at node=5
plot(train.rpart.snip)
text(train.rpart.snip,splits=TRUE,use.n=TRUE)
post(train.rpart.snip,file="",title="Analytics Vidya Text mining-RPART",use.n=TRUE)

## Prediction on TEST and Train data set using Trained model
## Factors - Predicted
testpred.rpart.factor<-predict(train.rpart,Test_wtsp,type="class")
trainpred.rpart.factor<-predict(train.rpart,Train_wtsp,type="class")

##Misclassification Matrix
MisClassTest<-table("Predict"=testpred.rpart.factor,"Actual"=Test_wtsp$Author)  ## Test Data Prediction
MisClassTrain<-table("Predict"=trainpred.rpart.factor,"Actual"=Train_wtsp$Author)   ## Train Data Prediction

##Accuracy based on Acceptance criteria
accuracyCART<-(100-mean(c((nrow(Test_wtsp)-sum(diag(MisClassTest)))/nrow(Test_wtsp)),(nrow(Train_wtsp)-sum(diag(MisClassTrain)))/nrow(Train_wtsp)))
accuracyCART


######################## 3. Conditional Inference Trees   ################################

library(party)

##Train the Dataset
Train.ctree<-ctree(Author~.,data=Train_wtsp,controls = ctree_control(maxsurrogate = 2))
plot(Train.ctree)

##Prediction on TEST and Train data set using Trained model
TestPred.ctree<-predict(Train.ctree,Test_wtsp)
TrainPred.ctree<-predict(Train.ctree,Train_wtsp)

##Misclassification Matrix
MisClassTest<-table("Predict"=TestPred.ctree,"Actual"=Test_wtsp$Author)  ## Test Data Prediction
MisClassTrain<-table("Predict"=TrainPred.ctree,"Actual"=Train_wtsp$Author)   ## Train Data Prediction

##Accuracy based on Acceptance criteria
accuracyCtree<-(100-mean(c((nrow(Test_wtsp)-sum(diag(MisClassTest)))/nrow(Test_wtsp)),(nrow(Train_wtsp)-sum(diag(MisClassTrain)))/nrow(Train_wtsp)))
accuracyCtree

######################## 4. Weighted K Nearest Neighbour   ################################


library(kknn)

##Train the Dataset
wtsp.kknn<-train.kknn(Author~.,Train_wtsp,distance=5,kernel = c("triangular","gaussian","epanechnikov", "optimal"),kmax=9)
summary(wtsp.kknn)

##Prediction on TEST and Train data set using Trained model
testPred.kknn<-predict(wtsp.kknn,Test_wtsp)
trainPred.kknn<-predict(wtsp.kknn,Train_wtsp)

##Misclassification Matrix
MisClassTest<-table("Predict"=testPred.kknn,"Actual"=Test_wtsp$Author)  ## Test Data Prediction
MisClassTrain<-table("Predict"=trainPred.kknn,"Actual"=Train_wtsp$Author)   ## Train Data Prediction

##Accuracy based on Acceptance criteria
accuracykknn<-(100-mean(c((nrow(Test_wtsp)-sum(diag(MisClassTest)))/nrow(Test_wtsp)),(nrow(Train_wtsp)-sum(diag(MisClassTrain)))/nrow(Train_wtsp)))
accuracyCkknn


######################## 5. K Nearest Neighbour   ################################


library(class)

##Train the Dataset
Train_wtsp_M<-data.matrix(Train_wtsp[,-which(colnames(Train_wtsp)=="Author")])

##Prediction on TEST and Train data set using Trained model
Test_wtsp_Mat<-data.matrix(Test_wtsp[,-which(colnames(Test_wtsp)=="Author")])
Train_wtsp_Mat<-data.matrix(Train_wtsp[,-which(colnames(Train_wtsp)=="Author")])

TestPred.knn<-knn(Train_wtsp_M,Test_wtsp_Mat,(Train_wtsp$Author))
TrainPred.knn<-knn(Train_wtsp_M,Train_wtsp_Mat,(Train_wtsp$Author))

##Misclassification Matrix
MisClassTest<-table("Predict"=TestPred.knn,"Actual"=Test_wtsp$Author)  ## Test Data Prediction
MisClassTrain<-table("Predict"=TrainPred.knn,"Actual"=Train_wtsp$Author)   ## Train Data Prediction

##Accuracy based on Acceptance criteria
accuracyknn<-(100-mean(c((nrow(Test_wtsp)-sum(diag(MisClassTest)))/nrow(Test_wtsp)),(nrow(Train_wtsp)-sum(diag(MisClassTrain)))/nrow(Train_wtsp)))
accuracyknn


######################## 6. Random Forest Ensembles   ################################

##Train the Dataset
Train.RandomForestEnsemble<-cforest(Author~.,data=Train_wtsp,control=cforest_unbiased(ntree=4))

##Predict on Test Data and Train Data
TestPred.RandomForestEnsemble<-predict(Train.RandomForestEnsemble,Test_wtsp,OOB=TRUE)
TrainPred.RandomForestEnsemble<-predict(Train.RandomForestEnsemble,Train_wtsp,OOB=TRUE)

##Misclassification Matrix
MisClassTest<-table("Predict"=TestPred.RandomForestEnsemble,"Actual"=Test_wtsp$Author)  
MisClassTrain<-table("Predict"=TrainPred.RandomForestEnsemble,"Actual"=Train_wtsp$Author)   

##Accuracy based on Acceptance criteria
accuracyRF<-(100-mean(c((nrow(Test_wtsp)-sum(diag(MisClassTest)))/nrow(Test_wtsp)),(nrow(Train_wtsp)-sum(diag(MisClassTrain)))/nrow(Train_wtsp)))
accuracyRF
