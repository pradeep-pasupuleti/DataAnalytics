data <- read.table("natal2010Sample.tsv.gz", sep="\t", header=T, stringsAsFactors=F)
dim(data)
summary(data)
# y variable: atRisk = APGAR5 < 7  - APGAR5
# 1.8%, excluding unknowns
data$atRisk = with(data, ifelse(APGAR5==99, NA,APGAR5 < 7))

# make a boolean from Y/N data
makevarYN = function(col) {
  ifelse(col %in% c("", "U"), NA, ifelse(col=="Y", T, F))
}

# make a numeric var w/NAs from numeric data
makevarNum = function(col, sentinel) {
  ifelse(col==sentinel, NA, col)
}

# make a boolean from 1/2/9 data.
makevar12 = function(col) {
  ifelse(col==9, NA, ifelse(col==1, T, F))
}

# tobacco use: CIG_REC (Y, N, U, Blank)
data$CIG_REC = makevarYN(data$CIG_REC)
# maternal prepregnancy weight (pounds), capped at 400lbs
data$PWGT = makevarNum(data$PWGT, 999)
# weight gain during pregnancy
data$WTGAIN = makevarNum(data$WTGAIN, 99)
# birth weight in grams
data$DBWT = makevarNum(data$DBWT, 9999)

# complications:
# meconium: mod/heavy staining of amniotic fluid with fetal fecal matter
# precipitous labor = really short (membrane ruptures, etc)
# breech birth
complications = c("ULD_MECO","ULD_PRECIP","ULD_BREECH")
data[, complications] = as.data.frame(lapply(data[, complications], FUN=makevar12))

# obstetric procedures:
# tocolysis -- anti-contraction medication given to prevent premature labor
# induc -- labor was induced
obsproc = c("UOP_TOCOL", "UOP_INDUC")
data[, obsproc] = as.data.frame(lapply(data[, obsproc], FUN=makevar12))

# number of prenatal visits
data$UPREVIS = makevarNum(data$UPREVIS, 99)

#risk factors (1,2,9,Blank)
# diabetes, chronic hypertension, pregnancy-associated hypertension, eclampsia
riskfactors = c("URF_DIAB", "URF_CHYPER", "URF_PHYPER", "URF_ECLAM")
data[, riskfactors] = as.data.frame(lapply(data[, riskfactors], FUN=makevar12))

# reset the "default" level on categorical variabls
recode = function(col, map, ref) {
  relevel(as.factor(map[col]), ref=ref)
}

# gestation length
# GESTREC3 (1,2,3 -- <37weeks(premie), >=37wks, NA)
grmap = c("< 37 weeks",
          ">= 37 weeks",
          NA)
data$GESTREC3 = recode(data$GESTREC3, grmap, grmap[[2]])

# DPLURAL : birth plurality
plmap = c("single",
          "twin",
          "triplet or higher",
          "triplet or higher",
          "triplet or higher")
data$DPLURAL = recode(data$DPLURAL, plmap, "single")

# Select variables we will use for the analysis
y = "atRisk"
x = c("PWGT", 
      "UPREVIS", 
      "CIG_REC",
      "GESTREC3", 
      "DPLURAL",
      complications,
      riskfactors)
fmla = paste(y, paste(x, collapse="+"), sep="~")
fmla
sdata = data[, c(x, y, c("DBWT", "ORIGRANDGROUP"))]

# get rid of the NA data before splitting into train and test
# noNAs is T if there are no NAs in the row
noNAs = rowSums(as.data.frame(lapply(sdata, FUN=is.na))) == 0
sdata = sdata[noNAs, ]

save(sdata, file="NatalRiskData.rData")


train <- sdata[sdata$ORIGRANDGROUP<=5,]
test <- sdata[sdata$ORIGRANDGROUP>5,]
dim(train)
dim(test)
dim(sdata)
model <- glm(fmla, data=train, family=binomial(link="logit"))
summary(model)

train$pred <- predict(model, newdata=train, type="response")
test$pred <- predict(model, newdata=test, type="response")
library(ggplot2)
ggplot(train, aes(x=pred, color=atRisk, linetype=atRisk)) +
  geom_density()

library(ROCR)
library(grid)
predObj <- prediction(train$pred, train$atRisk)
precObj <- performance(predObj, measure="prec")
recObj <- performance(predObj, measure="rec")
precision <- (precObj@y.values)[[1]]
prec.x <- (precObj@x.values)[[1]]
recall <- (recObj@y.values)[[1]]
rocFrame <- data.frame(threshold=prec.x, precision=precision,
                       recall=recall)
nplot <- function(plist) {
  n <- length(plist)
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(n,1)))
  vplayout=function(x,y) {viewport(layout.pos.row=x, layout.pos.col=y)}
  for(i in 1:n) {
    print(plist[[i]], vp=vplayout(i,1))
  }
}
pnull <-mean(as.numeric(train$atRisk))
p1 <- ggplot(rocFrame, aes(x=threshold)) +
  geom_line(aes(y=precision/pnull)) +
  coord_cartesian(xlim = c(0,0.05), ylim=c(0,10) )

p2 <- ggplot(rocFrame, aes(x=threshold)) +
  geom_line(aes(y=recall)) +
  coord_cartesian(xlim = c(0,0.05) )
nplot(list(p1, p2))

#confusion matrix
ctab.test <- table(pred=test$pred>0.02, atRisk=test$atRisk)
ctab.test
precision <- ctab.test[2,2]/sum(ctab.test[2,])
recall <- ctab.test[2,2]/sum(ctab.test[,2])
enrich <- precision/mean(as.numeric(test$atRisk))
precision
recall
enrich

coefficients(model)
summary(model)

pred <- predict(model, newdata=train, type="response")
llcomponents <- function(y, py) {
  y*log(py) + (1-y)*log(1-py)
}
edev <- sign(as.numeric(train$atRisk) - pred) *
  sqrt(-2*llcomponents(as.numeric(train$atRisk), pred))
summary(edev)
#Computing deviance

loglikelihood <- function(y, py) {
  sum(y * log(py) + (1-y)*log(1 - py))
}
pnull <- mean(as.numeric(train$atRisk))
null.dev <- -2*loglikelihood(as.numeric(train$atRisk), pnull)
pred <- predict(model, newdata=train, type="response")
resid.dev <- -
  2*loglikelihood(as.numeric(train$atRisk), pred)
testy <- as.numeric(test$atRisk)
testpred <- predict(model, newdata=test,
                    type="response")
pnull.test <- mean(testy)
null.dev.test <- -2*loglikelihood(testy, pnull.test)
resid.dev.test <- -2*loglikelihood(testy, testpred)
#computing significance of the observed fit
df.null <- dim(train)[[1]] - 1
df.model <- dim(train)[[1]] -
  length(model$coefficients)
delDev <- null.dev - resid.dev
deldf <- df.null - df.model
#Estimate probability of seeing the observed difference in deviances under null model (the p-value)
#using chi-squared distribution.
p <- pchisq(delDev, deldf, lower.tail=F)

#Calculating pseudo r square
pr2 <- 1-(resid.dev/null.dev)
# Calculating Akaike information criterion
aic <- 2*(length(model$coefficients) -
            loglikelihood(as.numeric(train    $atRisk), pred))
