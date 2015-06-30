closeAllConnections()
rm(list=ls())

library(caTools)
library(randomForest)
library(ROCR)
library(tm)


train<-read.csv("DataSciencePrep/C1/Data/AV/Emppred/Orig/train.csv")
test<-read.csv("DataSciencePrep/C1/Data/AV/Emppred/Orig/test.csv")
trainexC<-train
trainexC$Category=NULL

combined<-rbind(trainexC,test)
combinedexVar1<-combined
combinedexVar1$Var1=NULL
combined<-combinedexVar1



combined$Vintage<-as.character(combined$Vintage)

combined$Vintage<-substr(combined$Vintage, 1, 2)

combined$Vintage<-as.numeric(combined$Vintage)

combined$Skills<-as.character(combined$Skills)


CorpusSkills = Corpus(VectorSource(combined$Skills))
CorpusSkills = tm_map(CorpusSkills, tolower)
CorpusSkills = tm_map(CorpusSkills, PlainTextDocument)
CorpusSkills = tm_map(CorpusSkills, removePunctuation)
CorpusSkills = tm_map(CorpusSkills, removeWords, stopwords("english"))
CorpusSkills = tm_map(CorpusSkills, stemDocument)

dtm = DocumentTermMatrix(CorpusSkills)
sparse = removeSparseTerms(dtm, 0.99)
SkillsWords = as.data.frame(as.matrix(sparse))

colnames(SkillsWords) = paste("Skills", colnames(SkillsWords))
colnames(SkillsWords) = make.names(colnames(SkillsWords))

combined$Skills=NULL



combined$UG_Education<-as.character(combined$UG_Education)

CorpusUGE = Corpus(VectorSource(combined$UG_Education))
CorpusUGE = tm_map(CorpusUGE, tolower)
CorpusUGE = tm_map(CorpusUGE, PlainTextDocument)
CorpusUGE = tm_map(CorpusUGE, removePunctuation)
CorpusUGE = tm_map(CorpusUGE, removeWords, stopwords("english"))
CorpusUGE = tm_map(CorpusUGE, stemDocument)

dtm = DocumentTermMatrix(CorpusUGE)
sparse = removeSparseTerms(dtm, 0.99)
UGEWords = as.data.frame(as.matrix(sparse))

colnames(UGEWords) = paste("UGE", colnames(UGEWords))
colnames(UGEWords) = make.names(colnames(UGEWords))

combined$UG_Education=NULL



combined$UG_College<-as.character(combined$UG_College)

CorpusUGC = Corpus(VectorSource(combined$UG_College))
CorpusUGC = tm_map(CorpusUGC, tolower)
CorpusUGC = tm_map(CorpusUGC, PlainTextDocument)
CorpusUGC = tm_map(CorpusUGC, removePunctuation)
CorpusUGC = tm_map(CorpusUGC, removeWords, stopwords("english"))
CorpusUGC = tm_map(CorpusUGC, stemDocument)

dtm = DocumentTermMatrix(CorpusUGC)
sparse = removeSparseTerms(dtm, 0.99)
UGCWords = as.data.frame(as.matrix(sparse))

colnames(UGCWords) = paste("UGC", colnames(UGCWords))
colnames(UGCWords) = make.names(colnames(UGCWords))

combined$UG_College=NULL




combined$PG_Education<-as.character(combined$PG_Education)

CorpusPGE = Corpus(VectorSource(combined$PG_Education))
CorpusPGE = tm_map(CorpusPGE, tolower)
CorpusPGE = tm_map(CorpusPGE, PlainTextDocument)
CorpusPGE = tm_map(CorpusPGE, removePunctuation)
CorpusPGE = tm_map(CorpusPGE, removeWords, stopwords("english"))
CorpusPGE = tm_map(CorpusPGE, stemDocument)

dtm = DocumentTermMatrix(CorpusPGE)
sparse = removeSparseTerms(dtm, 0.99)
PGEWords = as.data.frame(as.matrix(sparse))

colnames(PGEWords) = paste("PGE", colnames(PGEWords))
colnames(PGEWords) = make.names(colnames(PGEWords))

combined$PG_Education=NULL



combined$PG_College<-as.character(combined$PG_College)

CorpusPGC = Corpus(VectorSource(combined$PG_College))
CorpusPGC = tm_map(CorpusPGC, tolower)
CorpusPGC = tm_map(CorpusPGC, PlainTextDocument)
CorpusPGC = tm_map(CorpusPGC, removePunctuation)
CorpusPGC = tm_map(CorpusPGC, removeWords, stopwords("english"))
CorpusPGC = tm_map(CorpusPGC, stemDocument)

dtm = DocumentTermMatrix(CorpusPGC)
sparse = removeSparseTerms(dtm, 0.99)
PGCWords = as.data.frame(as.matrix(sparse))

colnames(PGCWords) = paste("PGC", colnames(PGCWords))
colnames(PGCWords) = make.names(colnames(PGCWords))

combined$PG_College=NULL





combined<-cbind(combined,SkillsWords,UGEWords,UGCWords,PGEWords,PGCWords)







trainP<-head(combined,nrow(train))
testP<-tail(combined,nrow(test))


trainP$Category<-train$Category

set.seed(100)

rf<-randomForest(Category~.,data=trainP,na.action = na.roughfix)

pr<-predict(rf,newdata=testP)

pr<-as.character(pr)





#submission
sub=cbind(as.character(test$Var1),pr)
sub=data.frame(sub)
names(sub)=c("Var1","Category")




write.csv(sub, "sub.csv", row.names=FALSE)