library(datasets)
library("car")
data(attitude)
names(attitude)
class(attitude)
plot(attitude)
stnadattitude<-scale(attitude, FALSE, TRUE)
stnadattitude
sapply(stnadattitude,sd)
sapply(stnadattitude,mean)
dim(stnadattitude)
dim(attitude)

mosthighlycorrelated(attitude, 7)

indvar=attitude[2:7]
names(indvar)
class(indvar)
plot(indvar)
                   
mosthighlycorrelated(indvar, 6)
covariance=cov(indvar,method=c("pearson"))
covariance
correlation=cor(indvar,method=c("pearson"))
correlation
summary(indvar)
sapply(indvar,sd)



# 1st way of ghetting the pca using princomp- observe that we are not scaling

pcdat=princomp(indvar)
summary(pcdat)
plot(pcdat)
loadings(pcdat)
sum((pcdat$sdev)^2)

# 2nd way of getting the PCA using prcomp using SCALE true. This works fantastic as the zscore
# of the scaling ensures that the variance is set to 1 and mean is set to ~=0. 
#the result of scaling verified by sum((pcdat2$sdev)^2)  returns 6.
pcdat2=prcomp(indvar, scale=TRUE)

summary(pcdat2)
plot(pcdat2)
sum((pcdat2$sdev)^2)
pcdat$scores

#the following scaling is done as a research.not sure why i am not getting the correct result---TBD
#stdindvar<-scale(indvar,center=FALSE, scale=TRUE)
stdindvar<- as.data.frame(scale(attitude[2:7], center = FALSE, scale = apply(x, 2, sd, na.rm = TRUE)))
sapply(stdindvar,sd)
sapply(stdindvar,mean)

pcdatscaled=princomp(stdindvar)
summary(pcdatscaled)
plot(pcdatscaled)
print("third method")
sum((pcdatscaled$sdev)^2)
