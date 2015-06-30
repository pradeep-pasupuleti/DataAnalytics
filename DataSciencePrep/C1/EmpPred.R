library(rpart)
library(tm)
library(data.table)
sink('analysis-output.txt')

Emps=read.table("DataSciencePrep/C1/Data/AV/Emppred/train.csv",header=T,sep=",",quote = "\"",allowEscapes=TRUE,fill=TRUE)
Empstest=read.table("DataSciencePrep/C1/Data/AV/Emppred/test.csv",header=T,sep=",",quote = "\"",allowEscapes=TRUE,fill=TRUE)
attach(Emps)
fix(Emps)

ls()
# Basic Data summary
sink('analysis-output.txt', append=TRUE)
cat("# Basic Data summary testdata \n")
class(Empstest)
names(Empstest)
summary(Empstest)
dim(Empstest)
str(Empstest)
emptestcls_list <- lapply(Empstest, class)
cat("# Basic Data summary train \n")
summary(Emps)
names(Emps)
str(Emps)
empcls_list <- lapply(Emps, class)
sink()

sink('analysis-output.txt', append=TRUE)
cat("Understand how the variables are distributed \n")
#Trying to understand how the variables are distributed
hist(as.numeric(Var4))
hist(as.numeric(Var3))
hist(as.numeric(Var2))
hist(as.numeric(Category))
hist(as.numeric(UG_Education))
hist(as.numeric(Vintage.m))
hist(as.numeric(UG_College))
hist(Years)
apply(Vintage.m,2,mean)
hist(Months)
# canCoerce(Var4,"factor")
canCoerce(Var4,"numeric")

is.factor(Var4)
levels(Var4)
summary(Var4)
levels(UG_College)
levels(Vintage.m)
# top10<-head(Emps, n=10)
# fix(top10)
sink()
# Find if there are missing values
# do it for the test data set
sink('analysis-output.txt', append=TRUE)
cat("# Missing value analysis.Start with Test data set \n")

length(which(is.na(Empstest$Var1)))
length(which(is.na(Empstest$Var2)))
length(which(is.na(Empstest$Var3)))
length(which(is.na(Empstest$Vintage.m)))
length(which(is.na(Empstest$UG_Education)))
length(which(is.na(Empstest$UG_College)))
length(which(is.na(Empstest$PG_Education)))
length(which(is.na(Empstest$PG_College)))
length(which(is.na(Empstest$Skills)))
length(which(is.na(Empstest$Domain)))
length(which(is.na(Empstest$Var4)))
length(which(is.na(Empstest$Category)))
length(which(Empstest$Domain==""))

#now doit for the train data set
cat("# Missing value analysis.Start doing it on the train  data set \n")
is.na(Var1)
is.na(Var2)
length(which(is.na(Var1)))
length(which(is.na(Var2)))
length(which(is.na(Var3)))
length(which(is.na(Vintage.m)))
length(which(is.na(UG_Education)))
length(which(is.na(UG_College)))
length(which(is.na(PG_Education)))
length(which(is.na(PG_College)))
length(which(is.na(Skills)))
length(which(is.na(Domain)))
length(which(is.na(Var4)))
length(which(is.na(Category)))

length(unique(Var1))
table(UG_College)
sink()
# fnsum_unique<-function(Empss){
#   if(is.numeric(Empss))
#   {
#     sum(unique(Empss))
#   }
#   if(is.factor(Empss))
#   {
#     sum(levels(Empss))
#   }
#   # if(is.character(Empss))
#   # {
#   #  xx<- 100
#   # }
# }
sink('analysis-output.txt', append=TRUE)
cat("# Missing value analysis and treatment \n")

# found that the UG_College has 21 null values. replace by N/A
#y[is.na(y)] <- mean(y,na.rm=TRUE)
#Repaced by pradeep - just a dummy string so that when we convert in to a
#numeric value wewillassign 0 to pradeep remove null valuesfrom New age
UG_College<-as.factor(UG_College)
lapply(Emps,fnsum_unique)
unique(UG_College)
class(UG_College)
UG_College<-as.character(UG_College)
UG_College[is.na(UG_College)]<-as.character("pradeep", na.rm=TRUE)
length(which(UG_College=="pradeep"))

# do the missing value treatment to test dataset
cat("# do the missing value treatment to test dataset\n")
class(Empstest$UG_College)
Empstest$UG_College<-as.character(Empstest$UG_College)
Empstest$UG_College[is.na(Empstest$UG_College)]<-as.character("pradeep", na.rm=TRUE)
length(which(Empstest$UG_College=="pradeep"))
Empstest$PG_Education<-as.character(Empstest$PG_Education)
Empstest$PG_Education[is.na(Empstest$PG_Education)]<-as.character("PG College", na.rm=TRUE)
Empstest$PG_Education<-as.factor(Empstest$PG_Education)
length(which(Empstest$PG_Education=="PG College"))

#Fix for the problem in whiich there are an inordinate number of  blank elements
# TestDomain.C
#       1Do  2Do  3Do  4Do  5Do  6Do 
# 6668   36    2    1    2    1   14 
Empstest$Domain<-as.character(Empstest$Domain)
length(which(is.na(Empstest$Domain)))
Empstest$Domain[is.na(Empstest$Domain)]<-as.character("NA Domian",na.rm=TRUE)

#convert the vintage NAsto mean
Vintage.m[is.na(Vintage.m)] <-mean(Vintage.m,na.rm=TRUE)
which(is.na(Vintage.m))

#convert test the vintage NAsto mean
class(Empstest$Vintage.m)
Empstest$Vintage.m[is.na(Empstest$Vintage.m)] <-mean(Empstest$Vintage.m,na.rm=TRUE)
which(is.na(Empstest$Vintage.m))

length(which(Empstest$Domain==""))
class(Empstest$Domain)
Empstest$Domain<-as.character(Empstest$Domain)
Empstest$Domain[Empstest$Domain==""]<-as.character("No Domain",na.rm=TRUE) # We shoould actually do it with mode.
Empstest$Domain<-as.factor(Empstest$Domain)
cat("done with the missing value treatment")
sink()

# UG_Education<-as.character(UG_Education)
# class(UG_Education)
# length(unique(UG_Education))
# hist(table(UG_Education))
# hist(as.numeric(UG_Education))


#top101<-head(Emps,101)
#create dummy variablesbased on unique values
#cbind(top101, model.matrix(~top101$UG_Education-1))
# top101

sink('analysis-output.txt', append=TRUE)
cat("# Text clustering to examine if leventshien distance canbe used to categorzie \n")
# Text clustering to examine if leventshien distance canbe used to categorzie
set.seed(1)
rstr <- function(n,k){   # vector of n random char(k) strings
  sapply(1:n,function(i){do.call(paste0,as.list(sample(letters,k,replace=T)))})
}

fn_create_cluster<-function(input_df){
  strclust<- as.character(input_df)
  # Levenshtein Distance
  leven_d  <- adist(strclust)
  rownames(leven_d) <- strclust
  hc <- hclust(as.dist(leven_d))
  plot(hc)
  rect.hclust(hc,  k=8)
  output_df <- data.frame(strclust,cutree(hc,k=8))
  
}

output_df<-fn_create_cluster(Emps$Var2)
table(output_df)
dim(output_df)
#fix(df)
names(output_df)
sum(output_df$cutree.hc..k...8.==1)
sum(output_df$cutree.hc..k...8.==2)
sum(output_df$cutree.hc..k...8.==3)
sum(output_df$cutree.hc..k...8.==4)
sum(output_df$cutree.hc..k...8.==5)
sum(output_df$cutree.hc..k...8.==6)
sum(output_df$cutree.hc..k...8.==7)
sum(output_df$cutree.hc..k...8.==8)
cat("Clustering ends here- we have to explore better ways to do this. Details are documented on the areas of improvement")
sink()

#you can use Find and Replace in Excel to remove Year(s)/Month(s) and then
#insert a separator. Then using Text To Columns separate out Years & Months.
#Post that multiply Years by 12 and adding back Months which will give you total
#months. Few records have '+' sign please replace them manually. 

#Vintage.m is the new variable that we have created after doing the above



#quick understanding if there is a correlation.
cat("quick understanding if there is a correlation. usinng pairs \n")
pairs(top10)
sink()
# top10

nRowEmpstest<-nrow(Empstest)
nRowEmps<-nrow(Emps)

sink('analysis-output.txt', append=TRUE)

cat("Feature engineering.\n")
cat("# Code forcategorizing uge for Testing dataset\n")

# Feature engineering.
# Code forcategorizing uge for Testing dataset
# Testing how to convert text cols to bag of words
CorpusSkills<-NULL
CorpusSkills = Corpus(VectorSource(Emps2test$Skills))
CorpusSkills = tm_map(CorpusSkills, tolower)
CorpusSkills = tm_map(CorpusSkills, PlainTextDocument)
CorpusSkills = tm_map(CorpusSkills, removePunctuation)
CorpusSkills = tm_map(CorpusSkills, removeWords, stopwords("english"))
CorpusSkills = tm_map(CorpusSkills, stemDocument)

dtm = DocumentTermMatrix(CorpusSkills)
sparse = removeSparseTerms(dtm, 0.99)
SkillsWordstest = as.data.frame(as.matrix(sparse))

colnames(SkillsWordstest) = paste("Skills", colnames(SkillsWordstest))
colnames(SkillsWordstest) = make.names(colnames(SkillsWordstest))

Emps3test<-cbind(Emps2test,SkillsWordstest)
Emps3test<-subset(Emps3test,select=-c(PG_College,Var1,Skills,Var2,Vintage,Var3,Var4,UG_Education,PG_Education,UG_College,Domain,years,months,uge,pge,x))
str(Emps3test)

CorpusSkills<-NULL

CorpusSkills = Corpus(VectorSource(Emps$Skills))
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

Emps3<-cbind(Emps2,SkillsWords)
Emps3<-subset(Emps3,select=-c(Var1,Var2,Var3,Var4,UG_Education,PG_Education,UG_College,Domain,Years,Months,uge,pge,x))
Emps3<-subset(Emps3,select=-c(PG_College))

Emps$UG_Education<-as.character(Emps$UG_Education)

CorpusUGE = Corpus(VectorSource(Emps$UG_Education))
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

CorpusUGC = Corpus(VectorSource(Emps$UG_College))
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



CorpusPGE = Corpus(VectorSource(Emps$PG_Education))
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

CorpusPGC = Corpus(VectorSource(Emps$PG_College))
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

# Code forcategorizing uge for Training dataset
cat("# Code forcategorizing uge for Training dataset\n")
regexp <- "^M\\W"
Ug_Education.c <- data.frame(uge = character(nRowEmps),stringsAsFactors = FALSE)
for(i in 1:nRowEmps) {
  strCmp<-Emps$UG_Education[i]
  if(strCmp == "B.Tech/B.E.") {
    Ug_Education.c$uge[i]<-"1UG"
  } else if(grepl(pattern = regexp, x = strCmp)) {
    Ug_Education.c$uge[i]<-"2UG"
  } else if(strCmp == "B.Sc") {
    Ug_Education.c$uge[i]<-"3UG"
  } else if(strCmp == "B.B.A "|| strCmp == "BCA" || strCmp == "Diploma") {
    Ug_Education.c$uge[i]<-"4UG"
  } else if(strCmp == "B.Com"|| strCmp == "B.A" || strCmp == "B.Pharma") {
    Ug_Education.c$uge[i]<-"5UG"
  } else 
    Ug_Education.c$uge[i]<-"6UG"
}
table(Ug_Education.c)
# verify if the class we have got a char here and the dummy vars funbelow needs  a factor.hence converting to factor 
cat("# verify if the class we have got a char here and the dummy vars funbelow needs  a factor.hence converting to factor ")
class(Ug_Education.c)
class(Ug_Education.c$uge) 
Ug_Education.c$uge<-as.factor(Ug_Education.c$uge)
#now wegot a factor.assert below
class(Ug_Education.c$uge)
names(Emps2)

# bind the new df 
cat("# bind the new df")
Emps2<-cbind(Emps,Ug_Education.c)# this will add only uge to the columns

#Sanity check if the addition is indeed good
cat("#Sanity check if the addition is indeed good\n")
names(Emps2)
class(Emps2$uge)
table(Emps2$uge)

#Create dummy variables from uge
cat("#Create dummy variables from uge\n")
#cbind(Emps2, model.matrix(~Emps2$uge-1))# covert into modelmatrix somehowthis is not working. Workaround below
for(level in unique(Emps2$uge)){
  Emps2[paste("dummy", level, sep = "_")] <- 
    ifelse(Emps2$uge == level, 1, 0)
}
#verify if the dummy variablesare indeed the same as in the new df pge
cat("#verify if the dummy variablesare indeed the same as in the new df pge\n")
names(Emps2)
table(Emps2$uge)
table(Emps2$dummy_1UG)
table(Emps2$dummy_2UG)
table(Emps2$dummy_3UG)
table(Emps2$dummy_4UG)
table(Emps2$dummy_5UG)
table(Emps2$dummy_6UG)
table(Emps2$uge)

sink()

cat("# # In Train data Code forcategorizing Post grad edu \n")

Pg_Education.c <- data.frame(pge = character(nRowEmps),stringsAsFactors = FALSE)
for(i in 1:nRowEmps) {
  strCmp<-Emps$PG_Education[i]
  if(grepl(pattern = strMasterregexp, x = strCmp)|| grepl(pattern = strPostGradregexp, x = strCmp) || grepl(pattern = strPGGradregexp, x = strCmp)) { 
    Pg_Education.c$pge[i]<-"1PG"
  } else if(grepl(pattern = strCAregexp, x = strCmp) || grepl(pattern = strCFAregexp, x = strCmp) || grepl(pattern = strCHartregexp, x = strCmp)) {
    Pg_Education.c$pge[i]<-"2PG"
  } else if(strCmp == "N/A") {
    Pg_Education.c$pge[i]<-"4NAPG"
  } else 
    Pg_Education.c$pge[i]<-"3PG"
}
# drop the dummy columns  Not sure which works well...imean efficiently dont use it on realdata.
# Emps2test <- subset(Emps2test, select = -c(grepl( "dummy" , names( Emps2test ) ) )) This does not work
# Emps2test<-Emps2test[,-grepl( "^dummy\\W" , colnames( Emps2test ) )] Hopethhis works better
# names(Emps2test)
# colnames(Emps2test)
# #return only the dummy columns
# head(Emps2test[,grepl( "dummy" , names( Emps2test ) )])
# table(Pg_Education.c)
# Pg_Education.c
# 1PG   2PG   3PG 4NAPG 
# 1327     9  5744  2807 
# The answeris wrong as the regexs are overacting, || is the problem. For the time being lets park this here and moveon

# Check if the new df has been populated properly
cat("# Check if the new df has been populated properly\n")
table(Pg_Education.c)
class(Pg_Education.c)
names(Emps2)
# Bind the newdataframe w
cat("# Bind the newdataframe w \n")
Emps2<-cbind(Emps2,Pg_Education.c) # note that only df.xor pge is binded in reality
# sanity here
cat("sanity chekif Binding was successful\n")
names(Emp2)
table(Emps2$pge)

# Create dummy variablesfrom pge factor levels
cat("# Create dummy variablesfrom pge factor levels\n")
for(level in unique(Emps2$pge)){
  Emps2[paste("dummy", level, sep = "_")] <- 
    ifelse(Emps2$pge == level, 1, 0)
}

#verify if the dummy variablesare indeed the same as in the new df pge

cat("#verify if the dummy variablesare indeed the same as in the new df pge \n")
table(Emps2$pge)
table(Emps2$dummy_1PG)
table(Emps2$dummy_2PG)
table(Emps2$dummy_3PG)
table(Emps2$dummy_4NAPG)


cat("Done with creation of dummy variablesfor PGEof train data\n")
sink()

#Start with categorizing the domain in to 5-6 major categories
cat("#In Train data Create dvars for domain too\n ")
cat("#Start with categorizing the domain in to 5-6 major categories\n")

Domain.C <- data.frame(x = character(nRowEmps),stringsAsFactors = FALSE) 
for(i in 1:nRowEmps) {
  strCmp<-Emps$Domain[i]
  if(strCmp == "Analytics & Business Intelligence" || strCmp =="Top Management") {
    Domain.C$x[i]<-"1Do"
  } else if(grepl(pattern = strITregexp, x = strCmp)) {
    Domain.C$x[i]<- "2Do"
  } else if(strCmp == "Accounts / Finance / Tax / CS / Audit") {
    Domain.C$x[i]<- "3Do"
  } else if(strCmp == "Marketing / Advertising / MR / PR "|| strCmp == "Banking / Insurance" || strCmp == "Sales / BD") {
    Domain.C$x[i]<- "4Do"
  } else if(strCmp == "Corporate Planning / Consulting"|| strCmp == "Pharma / Biotech / Healthcare / Medical / R&D" || strCmp == "Purchase / Logistics / Supply Chain") {
    Domain.C$x[i]<- "5Do"
  } else 
    Domain.C$x[i]<- "6Do"
}
table(Domain.C)
sum(as.numeric(Domain.C$x))

class(Domain.C)
class(Domain.C$x)
str(Domain.C)
dim(Domain.C)
Domain.C$x<-as.factor(Domain.C$x)
# This adds the new column - by the way it only addst the x not domain.c
Emps2<-cbind(Emps2,Domain.C)

# Sanity check here
names(Emps2)
str(Emps2)
class(Emps2$x)

# Create thge dummy variables from the categories of x
cat("# Create thge dummy variables from the categories of x \n")
# cbind(Emps2, model.matrix(~Emps2$Domain.C-1)) covert into modelmatrix but this thing aint working
for(level in unique(Emps2$x)){
  Emps2[paste("dummy", level, sep = "_")] <- 
    ifelse(Emps2$x == level, 1, 0)
}
#Saniy check
cat("Sanity Check the domain dummy variables")
table(Emps2$x)
table(Emps2$dummy_1Do)
table(Emps2$dummy_2Do)
table(Emps2$dummy_3Do)
table(Emps2$dummy_4Do)
table(Emps2$dummy_5Do)
table(Emps2$dummy_6Do)
names(Emps2)
dim(Emps2)

sink()


regexp <- "^M\\W"
Ug_EducationTest.c <- data.frame(uge = character(nRowEmpstest),stringsAsFactors = FALSE)
for(i in 1:nRowEmpstest) {
  strCmp<-Empstest$UG_Education[i]
  if(strCmp == "B.Tech/B.E.") {
    Ug_EducationTest.c$uge[i]<-"1UG"
  } else if(grepl(pattern = regexp, x = strCmp)) {
    Ug_EducationTest.c$uge[i]<-"2UG"
  } else if(strCmp == "B.Sc") {
    Ug_EducationTest.c$uge[i]<-"3UG"
  } else if(strCmp == "B.B.A "|| strCmp == "BCA" || strCmp == "Diploma") {
    Ug_EducationTest.c$uge[i]<-"4UG"
  } else if(strCmp == "B.Com"|| strCmp == "B.A" || strCmp == "B.Pharma") {
    Ug_EducationTest.c$uge[i]<-"5UG"
  } else 
    Ug_EducationTest.c$uge[i]<-"6UG"
}
table(Ug_EducationTest.c)
# verify if the class we have got a char here and the dummy vars funbelow needs  a factor.hence converting to factor 
class(Ug_EducationTest.c)
class(Ug_EducationTest.c$uge) 
Ug_EducationTest.c$uge<-as.factor(Ug_EducationTest.c$uge)
#now wegot a factor.assert below
class(Ug_EducationTest.c$uge)

cat("# bind the new df \n")
# bind the new df 
Emps2test<-cbind(Empstest,Ug_EducationTest.c)# this will add only uge to the columns
cat("#Sanity check if the addition is indeed good\n")
#Sanity check if the addition is indeed good
names(Emps2test)
class(Emps2test$uge)
table(Emps2test$uge)

#Create dummy variables from uge
cat("#Create dummy variables from uge\n")
#cbind(Emps2test, model.matrix(~Emps2test$uge-1))# covert into modelmatrix somehowthis is not working. Workaround below
for(level in unique(Emps2test$uge)){
  Emps2test[paste("dummy", level, sep = "_")] <- 
    ifelse(Emps2test$uge == level, 1, 0)
}
cat("#verify if the dummy variablesare indeed the same as in the new df pge\n")
#verify if the dummy variablesare indeed the same as in the new df pge
names(Emps2test)
table(Emps2test$uge)
table(Emps2test$dummy_1UG)
table(Emps2test$dummy_2UG)
table(Emps2test$dummy_3UG)
table(Emps2test$dummy_4UG)
table(Emps2test$dummy_5UG)
table(Emps2test$dummy_6UG)
table(Emps2test$uge)


cat("#done with Code forcategorizing uge for Testing dataset \n")
sink()

# Code forcategorizing pge
# var initiation

strMasterregexp <- "^M\\W"
strPostGradregexp <- "^Post Grad\\W"
strPGGradregexp <- "^PG\\W"
strCAregexp <- "^C\\W"
strCFAregexp <- "^CF\\W"
strCHartregexp<- "^Chartered\\W"

# Code for categorizing pge in Test data
cat("# # In Test data Code forcategorizing Post grad edu \n")
Pg_Educationtest.c <- data.frame(pge = character(nRowEmpstest),stringsAsFactors = FALSE)

is.NA(Empstest$PG_Education)
Empstest$PG_Education<-as.character(Empstest$PG_Education)
for(i in 1:nRowEmpstest) {
  strCmp<-Empstest$PG_Education[i]
  sprintf("the value of i is %d and the value of strCmp is %s", i,strCmp)
    if(grepl(pattern = strMasterregexp, x = strCmp)|| grepl(pattern = strPostGradregexp, x = strCmp) || grepl(pattern = strPGGradregexp, x = strCmp)) { 
    Pg_Educationtest.c$pge[i]<-"1PG"
  } else if(grepl(pattern = strCAregexp, x = strCmp) || grepl(pattern = strCFAregexp, x = strCmp) || grepl(pattern = strCHartregexp, x = strCmp)) {
    Pg_Educationtest.c$pge[i]<-"2PG"
  } else if(strCmp == "N/A") {
    Pg_Educationtest.c$pge[i]<-"4NAPG"
  } else 
    Pg_Educationtest.c$pge[i]<-"3PG"
}
# table(Pg_Educationtest.c)
# Pg_Educationtest.c
# 1PG   2PG   3PG 4NAPG 
# 1327     9  5744  2807 
# The answeris wrong as the regexs are overacting, || is the problem. For the time being lets park this here and moveon

# Check if the new df has been populated properly
cat("# Check if the new df has been populated properly\n")
table(Pg_Educationtest.c)
class(Pg_Educationtest.c$pge)

# Bind the newdataframe w
cat("# Bind the newdataframe w \n")
Emps2test<-cbind(Emps2test,Pg_Educationtest.c) # note that only df.xor pge is binded in reality
# sanity here
cat("sanity chekif Binding was successful\n")
class(Emps2test$pge)
names(Emps2test)
table(Emps2test$pge)
Emps2test$pge<-as.factor(Emps2test$pge)
# Create dummy variablesfrom pge factor levels
cat("# Create dummy variablesfrom pge factor levels\n")
for(level in unique(Emps2test$pge)){
  Emps2test[paste("dummy", level, sep = "_")] <- 
    ifelse(Emps2test$pge == level, 1, 0)
}

#verify if the dummy variablesare indeed the same as in the new df pge

cat("#verify if the dummy variablesare indeed the same as in the new df pge \n")
table(Emps2test$pge)
table(Emps2test$dummy_1PG)
table(Emps2test$dummy_2PG)
table(Emps2test$dummy_3PG)
table(Emps2test$dummy_4NAPG)
table(Emps2test$pge)

# 270are unaccounted this is one area of improvement that resultedin dummy_
#       1PG   2PG   3PG 4NAPG 
# 270   893     7  3730  1824 

cat("Done with creation of dummy variablesfor PGE of test data\n")
sink()



#Create dvars for test data "Domain" attribute
strITregexp <- "^IT\\W"

#Start with categorizing the domain in to 5-6 major categories

cat("#In Test data Create dvars for domain too\n ")
cat("#Start with categorizing the domain in to 5-6 major categories\n")

TestDomain.C <- data.frame(x = character(nRowEmpstest),stringsAsFactors = FALSE) 
for(i in 1:nRowEmpstest) {
  
  strCmp<-Empstest$Domain[i]
  if(strCmp == "Analytics & Business Intelligence" || strCmp =="Top Management") {
    TestDomain.C$x[i]<-"1Do"
  } else if(grepl(pattern = strITregexp, x = strCmp)) {
    TestDomain.C$x[i]<- "2Do"
  } else if(strCmp == "Accounts / Finance / Tax / CS / Audit") {
    TestDomain.C$x[i]<- "3Do"
  } else if(strCmp == "Marketing / Advertising / MR / PR "|| strCmp == "Banking / Insurance" || strCmp == "Sales / BD") {
    TestDomain.C$x[i]<- "4Do"
  } else if(strCmp == "Corporate Planning / Consulting"|| strCmp == "Pharma / Biotech / Healthcare / Medical / R&D" || strCmp == "Purchase / Logistics / Supply Chain") {
    TestDomain.C$x[i]<- "5Do"
  } else 
    TestDomain.C$x[i]<- "6Do"
}
table(TestDomain.C)
sum(as.numeric(TestDomain.C$x))

class(TestDomain.C)
class(TestDomain.C$x)
str(TestDomain.C)
dim(TestDomain.C)
TestDomain.C$x<-as.factor(TestDomain.C$x)
# This adds the new column - by the way it only addst the x not domain.c
Emps2test<-cbind(Emps2test,TestDomain.C)

# Sanity check here
names(Emps2test)
str(Emps2test)
class(Emps2test$x)

# Create thge dummy variables from the categories of x
cat("# Create thge dummy variables from the categories of x \n")
# cbind(Emps2test, model.matrix(~Emps2test$TestDomain.C-1)) covert into modelmatrix but this thing aint working
for(level in unique(Emps2test$x)){
  Emps2test[paste("dummy", level, sep = "_")] <- 
    ifelse(Emps2test$x == level, 1, 0)
}
#Saniy check
cat("Sanity Check the domain dummy variables")
table(Emps2test$x)
table(Emps2test$dummy_1Do)
table(Emps2test$dummy_2Do)
table(Emps2test$dummy_3Do)
table(Emps2test$dummy_4Do)
table(Emps2test$dummy_5Do)
table(Emps2test$dummy_6Do)
names(Emps2test)
dim(Emps2test)

sink()



# solved the error Can't have empty classes in y. 
#here y means that category variable has ablank value level with 0 values in it.
#         Champion    Megastar Rising Star    Rockstar      Rookie        Star 
# 0        2531         283        2606         270        3204         960 

class(Emps2$Category)
table(Emps2$Category)
droplevels(Emps2$Category)
Emps2$Category<-as.character(Emps2$Category)
#removed and added as factor
Emps2$Category<-factor(Emps2$Category)
length(which(Emps2$Category==""))
Emps2$Category[Emps2$Category==""]<-as.character("NoneCat",na.rm=TRUE)
#Codeto delete a column 
#Emps2 <- subset(Emps2, select = -c(Domain.C) )
# fullcolnames<-data.frame(names(Emps2))
# fullcolnames
# class(fullcolnames)
# lst1<-subset(fullcolnames,select= -c("Var1"))#,UG_Education,UG_College,PG_Education,PG_College,Skills,Domain))
sink('analysis-output.txt', append=TRUE)
cat("Creating the formula strings\n")
# Creating the formula from the columns. Hv to find a better way
lstskills<-unlist(strsplit("Emps2$Var2+Emps2$Var3+Emps2$Skills+Emps2$dummy_1Do+Emps2$dummy_2Do+Emps2$dummy_3Do+Emps2$dummy_4Do+Emps2$dummy_5Do+Emps2$dummy_6Do+Emps2$Vintage.m+Emps2$dummy_1UG+Emps2$dummy_2UG+Emps2$dummy_3UG+Emps2$dummy_4UG+Emps2$dummy_5UG+Emps2$dummy_6UG+Emps2$dummy_1PG+Emps2$dummy_2PG+Emps2$dummy_3PG+Emps2$dummy_4NAPG","\\+"))
lst<-unlist(strsplit("Emps2$Var2+Emps2$Var3+Emps2$dummy_1Do+Emps2$dummy_2Do+Emps2$dummy_3Do+Emps2$dummy_4Do+Emps2$dummy_5Do+Emps2$dummy_6Do+Emps2$Vintage.m+Emps2$dummy_1UG+Emps2$dummy_2UG+Emps2$dummy_3UG+Emps2$dummy_4UG+Emps2$dummy_5UG+Emps2$dummy_6UG+Emps2$dummy_1PG+Emps2$dummy_2PG+Emps2$dummy_3PG+Emps2$dummy_4NAPG","\\+"))
lst
y = "Emps2$Category"
strformumlaskills = paste(y, paste(lstskills, collapse="+"), sep="~")
strformumla = paste(y, paste(lst, collapse="+"), sep="~")
strformumla

bb<-("Emps2$Category~Emps2$Var2+Emps2$Var3+Emps2$dummy_1Do+Emps2$dummy_2Do+Emps2$dummy_3Do+Emps2$dummy_4Do+Emps2$dummy_5Do+Emps2$dummy_6Do+Emps2$Vintage.m+Emps2$dummy_1UG+Emps2$dummy_2UG+Emps2$dummy_3UG+Emps2$dummy_4UG+Emps2$dummy_5UG+Emps2$dummy_6UG+Emps2$dummy_1PG+Emps2$dummy_2PG+Emps2$dummy_3PG+Emps2$dummy_4NAPG")
bb
strformumlawithoutEmps<-gsub(pattern = "Emps2\\$", replacement = "",  x = strformumla) #note the \\ toescape $
strformumlawithoutEmps

strformumlawithoutEmpsSkills<-gsub(pattern = "Emps2\\$", replacement = "",  x = strformumlaskills) #note the \\ toescape $

strTestformumla <-gsub(pattern = "Emps2", replacement = "data_train",  x = strformumla)
strTestformumla
strTestformumla1 <-gsub(pattern = "Category", replacement = "data_train$Category",  x = strTestformumla)
strTestformumla1

strvaldformumla <-gsub(pattern = "Emps2", replacement = "data_validate",  x = strformumla)
strvaldformumla
strvaldformumla1 <-gsub(pattern = "Category", replacement = "data_validate$Category",  x = strvaldformumla)
strvaldformumla1

# Create train and validate data from Emps2(which is nothing but complete set of 9887 rowsof train data )
cat("Create train and validate data from Emps2(which is nothing but complete set of 9887 rowsof train data\n")
nTrnSampPercent = nRowEmps*0.6 # we can change this 
ntrainrows = sample(1:nRowEmps,nTrnSampPercent) # to take a random sample of 60% of the records for train

data_train = Emps2[ntrainrows,]
dim(data_train) # Sanity check

nvalidaterows = (1:nRowEmps) [-ntrainrows] # to take a random sample of 40% of the records for test data
data_validate = Emps2[nvalidaterows,]
dim(data_validate)

sink()
sink('analysis-output.txt', append=TRUE)
cat("=============================\n")
cat("Cat walk starts here.I meant Modeling \n")
cat("=============================\n")

#Create predictive model on the entire data

cat("=============================\n")
cat("Analysis of Decision trees \n")
cat("=============================\n")


# quick and dirty decision tree
rpart_emp=rpart(strformumlawithoutEmps,method="anova", data=data_train,control=rpart.control(minsplit=8,cp=0.001))
rpart_emp
#use rattle to plot the dt
library(rattle)
fancyRpartPlot(rpart_emp)
predict()
plot(rpart_emp,main= "Classification of employees", margin=0.15,uniform=TRUE)
text(rpart_emp, use.n=TRUE, all=TRUE, cex=.8,col="blue")
printcp(rpart_emp)
nprunemt=rpart_emp$cptable[which.min(rpart_emp$cptable[,"xerror"]),"CP"]
nprunemt
pmttree<- prune(rpart_emp,cp=0.03)
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
sink()

sink('analysis-output.txt', append=TRUE)
cat("=============================\n")
cat("Analysis of Random forest")
cat("=============================\n")
Domain.C<-NULL

library(randomForest)


# one of the solution for the proble, Error in if (n == 0) stop("data (x) has 0 rows") : 
#   argument is of length zero
# use as.formula()) for the string formed out of paste here is a bit of a difference between the character string and to formula object

# R> modelFormula
# [1] "Species  ~ ."
# R> as.formula(modelFormula)
# Species ~ .

# The reason is here having blanks
# table(Emps2$Category)
# 
#         Champion    Megastar Rising Star    Rockstar      Rookie        Star 
# 0        2531         283        2606         270        3204         960 

#dummyColnames<-Emps2[,-grepl( "^dummy_\\W" , colnames(Emps2))] # Hopethhis works better
names(dummyColnames)
length(which(is.na(Emps2$Vintage.m)))
length(which(Emps2$Vintage.m==""))
Emps2$Vintage.m[is.na(Emps2$Vintage.m)] <-mean(Emps2$Vintage.m,na.rm=TRUE)
Emps2$Category[is.na(Emps2$Category)]<-"Rising Star"
length(which(is.na(Emps2$Category)))
Emps2$Category[is.na(Emps2$Category)]<-"Rising Star"

class(Emps2$Category)
table(Emps2$Category)
Emps2$Category<-as.character(Emps2$Category)
#removed and added as factor
Emps2$Category<-factor(Emps2$Category)
length(which(Emps2$Category==""))
Emps2$Category[Emps2$Category==""]<-as.character("NoneCat",na.rm=TRUE)
length(which(Emps2$Category=="NoneCat"))

# Added the skills too to test the accuracy

class(Emps2$Skills)

Emps2$Skills<-as.character(Emps2$Skills)
length(which(Emps2$Skills==""))
Emps2$Skills[Emps2$Skills==""]<-as.character("Unskilled",na.rm=TRUE)

length(which(is.na(Emps2$Skills)))
Emps2$Skills[is.na(Emps2$Skills)]<-"Rising Star"

length(which(is.nan(Emps2$Skills)))
length(which(is.infinite(Emps2$Skills)))
length(which(is.finite(Emps2$Skills)))

head(Emps2$Skills)
Empkills<-as.data.frame(Emps2$Skills)

set.seed(400)
emp.rf<-randomForest(Category~.,data=Emps3,ntree=100,keep.forest=TRUE,importance=TRUE, rsq=TRUE,na.action=na.omit)


# original without skills dtm 46%
#emp.rf<-randomForest(as.formula(strformumlawithoutEmps),data=Emps2,ntree=100,keep.forest=TRUE,importance=TRUE, rsq=TRUE,na.action=na.roughfix)

plot(emp.rf, log="y")
importance(emp.rf)
print(emp.rf$rsq)
plot(emp.rf$rsq)
hist(treesize(emp.rf))

# original without skills dtm 46%
#rf_pr<-predict(emp.rf,newdata=Emps2test)
rf_pr<-predict(emp.rf,newdata=Emps3test)
rf_pr<-as.character(rf_pr)

head(rf_pr,100)

dfrfFinal1<-subset(Emps2test,select=c(Var1))
dfrfFinal1<- cbind(dfrfFinal1,Category=rf_pr)
Empsrftest=NULL
names(dfrfFinal1)
str((dfrfFinal1))
dim(dfrfFinal1)
write.csv(dfrfFinal1, 
          file="Emp_predictionsRF.csv", row.names=FALSE, quote=FALSE)
# Your file was uploaded successfully!! Your score is 46.26 percent based on a
# sample of your solution. You Predicted 6724 answers and solution required 6724
# answers
cat("Your file was uploaded successfully!! Your score is 46.26 percent based on a sample of your solution. You Predicted 6724 answers and solution required 6724 answers")
sink()

sink('analysis-output.txt', append=TRUE)
cat("=============================\n")
cat("Analysis of GLM")
cat("=============================\n")


emp.glm1 <- glm(strformumlawithoutEmps, data=data_train, family=binomial(link="logit"))
#emp.glm1 <- glm(strformumla, data=Emps2, family=binomial(link="logit"))
summary(emp.glm1)

# data_train$pred <- predict(emp.glm1, newdata=data_train, type="response")
# data_validate$pred <- predict(emp.glm1, newdata=data_validate, type="response")
# library(ggplot2)
# ggplot(data_train, aes(x=pred, color=Category, linetype=Category)) +
#   geom_density()
# 
# library(ROCR)
# library(grid)
# predObj <- prediction(data_train$pred, data_train$Category)
# precObj <- performance(predObj, measure="prec")
# recObj <- performance(predObj, measure="rec")
# precision <- (precObj@y.values)[[1]]
# prec.x <- (precObj@x.values)[[1]]
# recall <- (recObj@y.values)[[1]]
# rocFrame <- data.frame(threshold=prec.x, precision=precision,
#                        recall=recall)
# nplot <- function(plist) {
#   n <- length(plist)
#   grid.newpage()
#   pushViewport(viewport(layout=grid.layout(n,1)))
#   vplayout=function(x,y) {viewport(layout.pos.row=x, layout.pos.col=y)}
#   for(i in 1:n) {
#     print(plist[[i]], vp=vplayout(i,1))
#   }
# }
# pnull <-mean(as.numeric(data_train$Category))
# p1 <- ggplot(rocFrame, aes(x=threshold)) +
#   geom_line(aes(y=precision/pnull)) +
#   coord_cartesian(xlim = c(0,0.05), ylim=c(0,10) )
# 
# p2 <- ggplot(rocFrame, aes(x=threshold)) +
#   geom_line(aes(y=recall)) +
#   coord_cartesian(xlim = c(0,0.05) )
# nplot(list(p1, p2))
library(nnet)

sink('analysis-output.txt', append=TRUE)
cat("=============================\n")
cat("Analysis of Multinom Regression from nnet")
cat("=============================\n")
#data_train<-subset(data_train,select= -c(pred))
emp.multin <- multinom(strformumlawithoutEmps, data=Emps2)

cat("Printing Summary Multinorm Regression")
summary(emp.multin)
## extract the coefficients from the model and exponentiate
cat("extract the coefficients from the model and exponentiate")
exp(coef(emp.multin))

z <- summary(emp.multin)$coefficients/summary(emp.multin)$standard.errors
z

# 2-tailed z test
p <- (1 - pnorm(abs(z), 0, 1)) * 2
p

# Use predicted probabilities to help you understand the model. You
# can calculate predicted probabilities for each of our outcome levels using the
# fitted function. We can start by generating the predicted probabilities for
# the observations in our dataset and viewing the first few rows
         
cat("Fitted function resuts for predicted probabilities")
head(pp <- fitted(emp.multin))

# To examine the changes in predicted probability associated with one
# of our two variables, we can create small datasets varying one variable while
# holding the other constant. We will first do this holding write at its mean
# and examining the predicted probabilities for each level of ses.


cat("Testing on the validated dataset")
predict(emp.multin, newdata = data_validate, "probs")
# fileConn<-file("summary.glmtxt.txt")
# writeLines(c(as.character(summary.glmtxt)), fileConn)
# close(fileConn)

cat("Testing on the testing dataset")
predict(emp.multin, newdata = Emps2test, "probs")

#Function to predict multinomial logit choice model outcomes
# model = nnet class multinomial model
# newdata = data frame containing new values to predict
predictMNL <- function(model, newdata) {
  
  # Only works for neural network models
  if (is.element("nnet",class(model))) {
    # Calculate the individual and cumulative probabilities
    probs <- predict(model,newdata,"probs")
    cum.probs <- t(apply(probs,1,cumsum))
    
    # Draw random values
    vals <- runif(nrow(newdata))
    
    # Join cumulative probabilities and random draws
    tmp <- cbind(cum.probs,vals)
    
    # For each row, get choice index.
    k <- ncol(probs)
    ids <- 1 + apply(tmp,1,function(x) length(which(x[1:k] < x[k+1])))
    
    # Return the values
    return(ids)
  }
}
unique(Emps2$Category)
predvalues <- predictMNL(emp.multin,Emps2test)
unique(predvalues)
dfFinal<-NULL
Emps3test<-NULL
dfFinal<-subset(Emps2test,select=c(Var1))
dfFinal<- cbind(dfFinal,Category=predvalues)

#Champion Megastar Rising Star Rockstar Rookie Star
dfFinal$Category[dfFinal$Category==1]<-"Champion"
dfFinal$Category[dfFinal$Category==2]<-"Megastar"
dfFinal$Category[dfFinal$Category==3]<-"Rising Star"
dfFinal$Category[dfFinal$Category==4]<-"Rockstar"
dfFinal$Category[dfFinal$Category==5]<-"Rookie"
dfFinal$Category[dfFinal$Category==6]<-"Star"
names(dfFinal)
str((dfFinal))

write.csv(dfFinal, 
          file="Emp_predictions.csv", row.names=FALSE, quote=FALSE)
# Your file was uploaded successfully!! Your score is 35.74 percent based on a
# sample of your solution. You Predicted 6724 answers and solution required 6724
# answers
sink()
