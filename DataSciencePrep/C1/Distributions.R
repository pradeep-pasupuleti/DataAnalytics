#Understadning through R code the various types of distributrions
#Binomial, Poisson, Geometric, Normal

library(ggplot2)
x <- seq(from=-5, to=5, length.out=100) # the interval [-5 5]
f <- dnorm(x) # normal with mean 0 and sd 1
ggplot(data.frame(x=x,y=f), aes(x=x,y=y)) + geom_line()

# draw 1000 points from a normal with mean 0, sd 1
u <- rnorm(1000)
# plot the distribution of points,
# compared to normal curve as computed by dnorm() (dashed line)
ggplot(data.frame(x=u), aes(x=x)) + geom_density() +
  geom_line(data=data.frame(x=x,y=f), aes(x=x,y=y), linetype=2)

# --- estimate probabilities (areas) under the curve ---
# 50% of the observations will be less than the mean
pnorm(0)
# about 2.3% of all observations are more than 2 standard
# deviations below the mean
pnorm(-2)
# [1] 0.02275013\

# about 95.4% of all observations are within 2 standard deviations
# from the mean
pnorm(2) - pnorm(-2)
# [1] 0.9544997

# --- return the quantiles corresponding to specific probabilities ---
# the median (50th percentile) of a normal is also the mean
qnorm(0.5)
# [1] 0
# calculate the 75th percentile
qnorm(0.75)
# [1] 0.6744898
pnorm(0.6744898)
# [1] 0.75
# --- Illustrate the 75th percentile ---
# create a graph of the normal distribution with mean 0, sd 1
x <- seq(from=-5, to=5, length.out=100)
f <- dnorm(x)
nframe <- data.frame(x=x,y=f)
# calculate the 75th percentile
line <- qnorm(0.75)
xstr <- sprintf("qnorm(0.75) = %1.3f", line)

# the part of the normal distribution to the left
# of the 75th percentile
nframe75 <- subset(nframe, nframe$x < line)
# Plot it.
# The shaded area is 75% of the area under the normal curve
ggplot(nframe, aes(x=x,y=y)) + geom_line() +
  geom_area(data=nframe75, aes(x=x,y=y), fill="gray") +
  geom_vline(aes(xintercept=line), linetype=2) +
  geom_text(x=line, y=0, label=xstr, vjust=1)


# draw 1001 samples from a lognormal with meanlog 0, sdlog 1
u <- rlnorm(1001)
# the mean of u is higher than the median
mean(u)
# [1] 1.638628
median(u)
# [1] 1.001051

# the mean of log(u) is approx meanlog=0
mean(log(u))
# [1] -0.002942916
# the sd of log(u) is approx sdlog=1
sd(log(u))
# [1] 0.9820357

# generate the lognormal with meanlog=0, sdlog=1
x <- seq(from=0, to=25, length.out=500)
f <- dlnorm(x)
# generate a normal with mean=0, sd=1
x2 <- seq(from=-5,to=5, length.out=500)
f2 <- dnorm(x2)

# make data frames
lnormframe <- data.frame(x=x,y=f)
normframe <- data.frame(x=x2, y=f2)
dframe <- data.frame(u=u)

# plot densityplots with theoretical curves superimposed
p1 <- ggplot(dframe, aes(x=u)) + geom_density() +
  geom_line(data=lnormframe, aes(x=x,y=y), linetype=2)

p2 <- ggplot(dframe, aes(x=log(u))) + geom_density() +
  geom_line(data=normframe, aes(x=x,y=y), linetype=2)

# functions to plot multiple plots on one page
library(grid)
nplot <- function(plist) {
  n <- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout<-function(x,y) {viewport(layout.pos.row=x, layout.pos.col=y)}
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}

# this is the plot that leads this section.
nplot(list(p1, p2))