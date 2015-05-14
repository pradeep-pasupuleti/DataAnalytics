library(sampling)

data(swissmunicipalities)
names(swissmunicipalities)
summary(swissmunicipalities)
#removed all the categorical attributes
swissmunicipalities2=subset(swissmunicipalities,select=-c(CT,REG,Nom))
median_pop= sapply(swissmunicipalities2,median)
median_pop
mean_pop = sapply(swissmunicipalities2,mean)
mean_pop
#sampling with replacement
n=290
s=srswr(n,nrow(swissmunicipalities2))
sample =getdata(swissmunicipalities2,s)
median_srswr=sapply(sample,median)
median_srswr

mean_srswr=sapply(sample,mean)
mean_srswr

nrow(sample)

#sampling without replacemnent
srsworidx=srswor(n,nrow(swissmunicipalities2))
sample_sswor=getdata(swissmunicipalities2,srsworidx)
median_srswor=sapply(sample_sswor,median)
median_srswor
nrow(sample_sswor)


#create a new subset of data wih required attributes
library(infotheo)
discretize_vars=subset(swissmunicipalities2,select=c(Surfacescult, POPTOT))
discretize=discretize(discretize_vars,disc="equalfreq", nbins=4)
#Change the column names so that we dont want to remove the original
colnames(discretize)=c("Surfacescult2","POPTOT2")
#add the new colunms into the actual dataset
swissmunicipalities3=cbind(swissmunicipalities2,discretize)
# table(swissmunicipalities3$POPTOT2,swissmunicipalities3$Surfacescult2)
# table(swissmunicipalities$POPTOT,swissmunicipalities$Surfacescult)

swissmunicipalities4 = swissmunicipalities3[order(swissmunicipalities3$POPTOT2,swissmunicipalities3$Surfacescult2), ]
st=strata(swissmunicipalities4, stratanames=c("POPTOT2","Surfacescult2"), size=c(34,25,12,1,17,21,22,13,11,15,22,25,11,12,16,34), method="srswor")
#the sample is
sample=getdata(swissmunicipalities4, st)
#Check the sample sizes drawn in each strata
table(sample$POPTOT2, sample$Surfacescult2)
#Median values of each attribute
median_strata=sapply(sample,median)
median_strata

#Comparison
ctable=cbind(median_pop,median_srswr,median_srswor[2:22],median_strata[1:21])
colnames(ctable) = c( "Population", "SRSWR", "SRSWOR", "Stratified Sampling")
ctable
