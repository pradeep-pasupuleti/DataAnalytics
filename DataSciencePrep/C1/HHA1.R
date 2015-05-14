setwd("C:/Pradeep/From UHG/Pradeep HDBkp/CPEE/CSE 7301C/HHA")
track <- read.csv("track.csv")
summary(track)
ls()
str(track)
levels(track$Country)

head(track,n=10)
sapply(track[4:5],function(x) x*60)
trackold<-track
trackold
track[, 1:3] <- sapply(track[, 1:3],function(x) x/60)
head(track,n=10)
head(trackold,n=10)
names(track)
class(track)
plot(track)
tracknum<-track[1:8]
mosthighlycorrelated(tracknum, 8)
covariance=cov(tracknum,method=c("pearson"))
covariance
correlation=cor(tracknum,method=c("pearson"))
correlation
summary(tracknum)
sapply(tracknum,sd)

# 2nd way of getting the PCA using prcomp using SCALE true. This works fantastic as the zscore
# of the scaling ensures that the variance is set to 1 and mean is set to ~=0. 
#the result of scaling verified by sum((pcdat2$sdev)^2)  returns 8.

track.pca=prcomp(tracknum, scale=TRUE)

summary(track.pca)
plot(track.pca)
sum((track.pca$sdev)^2)
track.pca$scores

rattle()

