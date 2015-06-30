library (tm)
library(wordcloud)
library (SnowballC)

cname <- file.path(".","DataSciencePrep/C1/Data/corpus","target")

docs <- Corpus(DirSource(cname))

for (j in seq(docs))
{
  docs[[j]] <- gsub("/"," ",docs[[j]])
  docs[[j]] <- gsub("@"," ",docs[[j]])
}
docs <- tm_map(docs,tolower)
docs <- tm_map(docs,removeWords, stopwords("english"))
docs <- tm_map(docs,removeNumbers)
docs <- tm_map(docs,removePunctuation)
docs <- tm_map(docs,stripWhitespace)
# Error: inherits(doc, "TextDocument") is not TRUE The problem is that the
# functions tolower and trim won't necessarily return TextDocuments (it looks
# like the older version may have automatically done the conversion). They
# instead return characters and the DocumentTermMatrix isn't sure how to handle
# a corpus of characters.
docs <- tm_map(docs, PlainTextDocument)
dtm <- DocumentTermMatrix(docs)

m <- as.matrix(dtm)
v <- sort(colSums(m),decreasing=TRUE)
head(v,14)
words <- names(v)
d <- data.frame(word=words, freq=v)
wordcloud(d$word,d$freq,min.freq=5, random.color=TRUE)
