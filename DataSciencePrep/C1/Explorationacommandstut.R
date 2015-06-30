closeAllConnections()
rm(list=ls())
# Find the summary information about the data frame loaded using command such as
# read.csv, read.table etc.
messages_text <- read.table( file.choose(), sep=",", stringsAsFactors=FALSE)
str(messages_text)

# Change the name of the columns to desired names; At times, during loading, the text file 
# could start straight away with the data. And, when that happens, the features are names as V1, V2 etc. 
# Thus, it may be good idea to name the features appropriately.
names(messages_text) <- c( "type", "text" , "text", "text", "text", "text", "text", "text", "text")

# as.factor command is frequenctly used to derive the categorical features as factor. When loaded, 
# this variable is loaded as character vector. 
messages_text$type <- as.factor(messages_text$type)

# table command when used on variable of class, factor, gives number of occurences of 
# different categories
table(messages_text$type)

# prop.table command when used on categorical variable (of class, factor) gives the percentage occurences of
# different categories
prop.table(table(messages_text$type))*100
hist(prop.table(table(messages_text$type))*100)
# round command with prop.table gives the percentage occurence of categorical variable, 
# rounded by number of digits specified in the command
round(prop.table(table(messages_text$type))*100, digits=2)
