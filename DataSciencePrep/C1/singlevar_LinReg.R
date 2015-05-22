linear=read.table("singlevar_lr.csv",header=T,sep=",")
attach(linear)
summary(lm(Disp~Weight))
#F-statistic: 92.99 on 1 and 35 DF,  p-value: 2.177e-11
#t statistic, as we know, measures how many standard deviations away the observed variable is from the predicted variable
#t statistic here is that the 0 slope is over 9 SDs away from the predicted value. As we know it is then highly unlikely that slope is zero. Hence, the fit is
#good. So, the rule of thumb is that higher t values are always good.
#(Pr). What it is saying is that the probability that slope is zero is less than or equal to that value there. As you can see again, the probability is
#very low. Again, we know that the fit is good
#the R squared is an indication of how much of the variance of the dependent variable is explained by the independent variable.
#We want it to be as high as possible.