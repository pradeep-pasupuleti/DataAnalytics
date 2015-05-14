wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",
                   sep=",")
wine
library("car")
scatterplotMatrix(wine[2:6])
plot(wine$V4, wine$V5)
text(wine$V4, wine$V5, wine$V1, cex=0.7, pos=4, col="red")
makeProfilePlot <- function(mylist,names)
{
  require(RColorBrewer)
  # find out how many variables we want to include
  numvariables <- length(mylist)
  # choose 'numvariables' random colours
  colours <- brewer.pal(numvariables,"Set1")
  # find out the minimum and maximum values of the variables:
  mymin <- 1e+20
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  # plot the variables
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}

library(RColorBrewer)
names <- c("V2","V3","V4","V5","V6")
mylist <- list(wine$V2,wine$V3,wine$V4,wine$V5,wine$V6)
makeProfilePlot(mylist,names)

sapply(wine[2:14],sd)
cor.test(wine$V2, wine$V3)
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  # find the correlations
  cormatrix <- cor(mydataframe)
  # set the correlations on the diagonal or lower triangle to zero,
  # so they will not be reported as the highest ones:
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  # flatten the matrix into a dataframe for easy sorting
  fm <- as.data.frame(as.table(cormatrix))
  # assign human-friendly names
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  # sort and print the top n correlations
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
mosthighlycorrelated(wine[2:14], 10)

standardisedconcentrations <- as.data.frame(scale(wine[2:14]))
sapply(standardisedconcentrations,mean)
sapply(standardisedconcentrations,sd)
wine.pca <- prcomp(standardisedconcentrations)     
summary(wine.pca)
screeplot(wine.pca, type="lines")
screeplot(wine.pca, type="barplot")
sum((wine.pca$sdev)^2)
plot(wine.pca)

