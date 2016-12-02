#############################################################################
# Author : Omkar Kulkarni 
# Topic  : Data Mining and Big Data Science:Examination Project - Resit
# Date   : 31/08/2016
#############################################################################


##### Import Data #####
rm(list=ls()) # remove all variables
cat("\014") # clear console
#setwd("C:/Users/Omkar/OneDrive/Big Data and Data Mining/Summer_Project")

student <- read.csv("student-mat.csv", sep = ";", header = TRUE, 
                    na.strings = "NA",fill = TRUE, stringsAsFactors = TRUE )

#### Flags ####
showInfo = FALSE # do not show intermediate results
showPlots = TRUE # show plots
saveMainResults = TRUE # save some results to a file
TRAIN.TEST.RATIO = 0.85

# Check for missing data
sapply(student, function(x) sum(is.na(x)))
# No missing data
# visvulize missing data
library(Amelia)
missmap(student, main = "Missing values vs observed")
# Strange, no Missing values !! 
#fix(student)
#Manually skim through the data to look for unexpected values such as '?', 'NAA' etc 

## As we already know, G1 and G2 are not to be part of the analysis. 
## hence we remove them and make a new dataset student.n 
student.n <- student[,]
drops <- c("G1","G2")
student.n <- student[ , !(names(student) %in% drops)]
rm(drops)
##### End Of Import Data #####



##### Pre define functions #####
# Detecting outliers: Do not use standard deviation around the mean, 
# use absolute deviation around the median

DoubleMAD <- function(x, zero.mad.action="warn"){
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  x         <- x[!is.na(x)]
  m         <- median(x)
  abs.dev   <- abs(x - m)
  left.mad  <- median(abs.dev[x<=m])
  right.mad <- median(abs.dev[x>=m])
  if (left.mad == 0 || right.mad == 0){
    if (zero.mad.action == "stop") stop("MAD is 0")
    if (zero.mad.action %in% c("warn", "warn and na")) warning("MAD is 0")
    if (zero.mad.action %in% c(  "na", "warn and na")){
      if (left.mad  == 0) left.mad  <- NA
      if (right.mad == 0) right.mad <- NA
    }
  }
  return(c(left.mad, right.mad))
}

DoubleMADsFromMedian <- function(x, zero.mad.action="warn"){
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  two.sided.mad <- DoubleMAD(x, zero.mad.action)
  m <- median(x, na.rm=TRUE)
  x.mad <- rep(two.sided.mad[1], length(x))
  x.mad[x > m] <- two.sided.mad[2]
  mad.distance <- abs(x - m) / x.mad
  mad.distance[x==m] <- 0
  return(mad.distance)
}

# DEFINE FUNCTIONS to calculate prediction statistics

CalculatePredictionStatistics2 <- function(set, method, response.values, predictions) {
  MSPE = mean((response.values - predictions)^2)
  SSE = sum((response.values - predictions)^2)
  SSTO = sum((response.values - mean(response.values))^2)
  R2 = (1 - SSE/SSTO) * 100
  MAPE = mean(abs(response.values - predictions)) # Mean absolute prediction error
  
  if (!exists("PredictionStatistics")) PredictionStatistics = NULL
  
  # remove statistics with the same method, but old data
  PredictionStatistics <<- PredictionStatistics[!(PredictionStatistics$set == set & 
                                                    PredictionStatistics$method == method),]
  
  NewRow = data.frame(set=set, method=method, MSPE=MSPE, R2=R2, MAPE=MAPE, stringsAsFactors = FALSE)
  PredictionStatistics <<- rbind(PredictionStatistics, NewRow)

}


# this function receives predictions as input and calculates some statistics like
# MSPE, R2
CalculatePredictionStatistics <- function(method, predictions.train, predictions.test) {
  
  response.values.train = student.train$G3
  response.values.test = student.test$G3
  
  CalculatePredictionStatistics2("train", method, response.values.train, predictions.train)
  CalculatePredictionStatistics2("test",  method, response.values.test,  predictions.test)
 
  predictions.train <<- NULL; predictions.test <<- NULL
  rm(predictions.train); rm(predictions.test)
  return(PredictionStatistics)
}

##### End Of Pre define functions #####


##### Univariate Data Exploration #####
#### Data Exploration 
plot(student.n$school, col = "blue", ylim=c(0,500), main="Schools")
plot(student.n$sex, col = "blue", ylim=c(0,500), main="Gender")
hist(student$age, col = "grey", main = "Age distribution", breaks = 8)
# The distribution of age is rather a bit odd, 
# we see some large values for age, as students more than 19 years old is 
# Odd we check for outliers in Age. We use MAD to check for outliers 
table(student.n$age)
barplot(table(student.n$age), main = "Age distribution", xlab = "age",
        ylab = "frequency")

TEMP = student.n$G3[student.n$age>18]
barplot(table(TEMP), ylim = c(0,60), main = "G3 of students with Age>18")
rm(TEMP)
# we see nothing surprising with AGE>18 vs G3 

## MAD for Age 
#OUTLIERS IN AGE
print(student$age[DoubleMADsFromMedian(student$age) > 3])
plot(student$age,student$G3, main = "Age Vs G3")
## Observe the G3 value for the age>20 namely for the outliers 21 and 22 years of age. 

#DROP THE ROWS WITH OUTLIERS IN AGE
student.n <- student.n[!(student.n$age>20),]

boxplot(student$age, col = "grey", main = "Age distribution before removing outliers")
boxplot(student.n$age, col = "grey", main = "Age distribution after removing outliers")

plot(student.n$address, col = "blue", ylim=c(0,500), main="Address", 
     xlab="'U' - urban or 'R' - rural")
plot(student.n$famsize, col = "blue", ylim=c(0,500), main="Family Size", 
     xlab="(LE3 - less or equal to 3 or GT3 - greater than 3")
plot(student.n$Pstatus, col = "blue", ylim=c(0,500), main="Parent's cohabitation status", 
     xlab="T' - living together or 'A' - apart")
hist(student.n$Medu, col = "grey", breaks = 6, main = "Mother's Education",
     xlab = "0 - none, 1 - primary education (4th grade), 2 - 5th to 9th
     grade, 3 - secondary education or 4 - higher education")
hist(student.n$Fedu, col = "grey", breaks = 6, main = "Father's Education",
     xlab = "0 - none, 1 - primary education (4th grade), 2 - 5th to 9th
     grade, 3 - secondary education or 4 - higher education")
barplot(table(student.n$Mjob), main="Mother's job", ylim = c(0,200))
barplot(table(student.n$Fjob), main="Father's job", ylim = c(0,200))
barplot(table(student.n$reason), main="Reason to choose school", ylim = c(0,180))
barplot(table(student.n$guardian), main="Guardian")
barplot(table(student.n$traveltime), main="Travel time to school", 
        xlab = "1 - <15 min., 2 - 15 to 30 min., 3 - 30 min. to 1
        hour, or 4 - >1 hour")
barplot(table(student.n$studytime), main="Study Time", 
        xlab = "1 - <2 hours, 2 - 2 to 5 hours, 3 - 5 to 10 hours, or 4 - >10
        hours")

cor(student.n$studytime, student.n$G3)
## strangely, correlation of study time and G3 is quite low= 0.10

barplot(table(student.n$failures), main="Number of past class failures", 
        xlab = "n if 1<= n <3, else 4")
cor(student.n$failures, student.n$G3)
## As expected, a negative correlation and relatively OK of -0.36

plot(student.n$schoolsup, col="blue", main="Extra educational support")
table(student.n$schoolsup, student.n$G3)

## now we see 8 Binary variables, YES and NO!! not plotting all of them. 
barplot(table(student.n$famrel), main="quality of family relationships", 
        xlab = "1 - very bad to 5 - excellent")

## distribution of final grade 
hist(student.n$G3, main = "Distribution of Final Grades")
barplot(table(student.n$G3), main = "Final Grade", xlab = "from 0 to 20")
summary(student$G3)
sd(student$G3)
boxplot(student.n$G3)

#OUTLIERS IN G3
print(student$G3[DoubleMADsFromMedian(student$G3) > 3])

## Zeros are indeed part of dataset and they are almost 10% of them. cannot possibly remove them. 

## test for normality ; which obviously does not look normal. 
shapiro.test(student.n$G3)
qqnorm(student.n$G3)
qqline(student.n$G3)
## certainly, G3 is NOT "Normally distributed", even Pvalue = 0.001 for Shapiro test.
## in the histogram we see a large number of 0's. 

t = student.n$G3[student.n$G3>0]
hist(t, main = "Non Zero scores")
shapiro.test(t)
qqnorm(t, main = "non zero scores")
qqline(t)
rm(t)
## even without the 0's it is not normal. 

##absences
hist(student.n$absences)
table(student.n$absences)
barplot(table(student.n$absences), main = "number of school absences", xlab = "days")
cor(student.n$absences, student.n$G3)

# The plot of "number of absences" is rather peculiar. The absences on 'Odd number of days' 
# is very low, it it does not follow the over all exponential pattern. The plot induces doubts 
# if the data collection for number of absences was correct
# it has weak correlation with G3.

#OUTLIERS IN absences
print(student$absences[DoubleMADsFromMedian(student$absences) > 3])
#outliers for exponential distributions make sense??? 
#no action to be taken on these outliers. 

#Scaled age and absences. 
student.s <- student.n
student.s$age <- (student.s$age - mean(student.s$age)) / sd(student.s$age)
student.s$absences <- (student.s$absences - mean(student.s$absences)) / sd(student.s$absences)

##### End of Univariate Data Exploration #####


##### Table plots #####
## check later, 
# explore entire data set as whole. 
require(tabplot)
tableplot(student,  sortCol = "G3")
# as expected we see strong corelation between G1,G2 and G3

#now plotting without G1 and G2 we see : 
tableplot(student.n,  sortCol = "G3")
## Plot for the most significant variables
tableplot(student.n, select = c(freetime, goout,studytime,reason
                                ,sex,age,Fjob,Mjob,health,failures,absences,31) , sortCol = "G3")
##### End of Table plots #####



##### Factor Analysis #####

# The term mixed refers to the simultaneous presence, as active elements, 
# of quantitative and qualitative variables. Roughly, we can say that FAMD works
# as a principal components analysis (PCA) for quantitative variables and as a
# multiple correspondence analysis (MCA) for qualitative variables.
library(FactoMineR)

#?FAMD()
Factor.analysis = FAMD(student.n, graph = TRUE, sup.var =31)
res.pca = PCA(student.n[,c(3,31,7,8,13,14,15,24,25,26,27,28,29,30)], graph = TRUE)
plot(Factor.analysis$quanti.var)
plot(Factor.analysis,habillage = 9)  # 9 is for Mjob
boxplot(student.n$G3~student.n$Mjob)

##### End Of Factor Analysis #####


####  scale and Split the data####
## 90% of the sample size

smp_size <- floor(TRAIN.TEST.RATIO * nrow(student.s))
## set the seed to make your partition reproductible
set.seed(123)
train_ind <- sample(seq_len(nrow(student.s)), size = smp_size)
student.train <- student.s[train_ind, ]
student.test <- student.s[-train_ind, ]

rm(smp_size, TRAIN.TEST.RATIO, train_ind)
rm(student,student.n,student.s)
####  End of scale and Split the data####

###############################################################################
###############################################################################
###############################################################################

########################## CREATE PREDICTION MODELS   #########################


#### OLS ####
student.model.lm = lm(G3~., data=student.train)
if (showInfo) summary(student.model.lm)
# calculate predictions
predictions.train = predict(student.model.lm, newdata=student.train)
predictions.test =  predict(student.model.lm, newdata=student.test)
CalculatePredictionStatistics("lm", predictions.train, predictions.test)
# this model overfits the training data

## tests
# Assessing Outliers
outlierTest(student.model.lm) # Bonferonni p-value for most extreme obs
qqPlot(student.model.lm, main="QQ Plot") #qq plot for studentized resid 

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(student.model.lm)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(student.model.lm)


## clearly the model assumptions are violated. 

keepAllVariables = TRUE
# OLS - selection of most significant variables
if (keepAllVariables)
{
  student.model.lm = lm(G3~sex+failures+famsup+activities+higher+
                          romantic+goout+Dalc+absences, data=student.train)
  # calculate predictions
  predictions.train = predict(student.model.lm, newdata=student.train)
  predictions.test = predict(student.model.lm, newdata=student.test)
  CalculatePredictionStatistics("lm2", predictions.train, predictions.test)
}

# clean up variables 
rm(student.model.lm)


#####Ridge ####
#glmnet standardizes the y variable and uses the mean squared errors 
#instead of sum of squared errors

library(glmnet)
x = model.matrix(G3~.,student.train)[,-31]
x.test = model.matrix(G3~.,student.test)[,-31]
# mode.matrix it also transforms any qualitative variables into dummy variables 
# apart from creating X matrix. 
y = student.train$G3

mcv_10fold <- cv.glmnet(x,y,alpha = 0 , nfolds = 10)
plot(mcv_10fold, xlab = "gamma", main="Ridge")
rm(mcv_10fold)

iteration=50
lambdas = NULL
for (i in 1:iteration)
{
  fit <- cv.glmnet(x,y,alpha = 0 , nfolds = 10)
  errors = data.frame(fit$lambda,fit$cvm, fit$lambda.1se)
  lambdas <- rbind(lambdas,errors)
}

# take mean cvm for each lambda
lambdas <- aggregate(lambdas[, 2:3], list(lambdas$fit.lambda), mean)

# select the best one
bestindex = which(lambdas[2]==min(lambdas[2]))
bestlambda = lambdas[bestindex,1]
# and now run glmnet once more with bestlambda


ridge.final.model <- glmnet(x,y,lambda=bestlambda, alpha = 0)

# calculate predictions
predictions.train = predict(ridge.final.model, newx = x) 
predictions.test = predict(ridge.final.model, newx = x.test) 
CalculatePredictionStatistics("ridge", predictions.train, predictions.test)

rm(iteration,lambdas,bestlambda,bestindex,predictions.test,predictions.train)  
rm(ridge.final.model)




##### Lasso  ####

Lasso_10fold <- cv.glmnet(x,y,alpha = 1 , nfolds = 10)
plot(Lasso_10fold, xlab = "gamma", main="Lasso")
rm(mcv_10fold)

iteration=50
lambdas = NULL
for (i in 1:iteration)
{
  fit <- cv.glmnet(x,y,alpha = 1 , nfolds = 10)
  errors = data.frame(fit$lambda,fit$cvm, fit$lambda.1se)
  lambdas <- rbind(lambdas,errors)
}

# take mean cvm for each lambda
lambdas <- aggregate(lambdas[, 2:3], list(lambdas$fit.lambda), mean)

# select the best one
bestindex = which(lambdas[2]==min(lambdas[2]))
bestlambda = lambdas[bestindex,1]
# and now run glmnet once more with bestlambda


lasso.final.model <- glmnet(x,y,lambda=bestlambda, alpha = 1)

# calculate predictions
predictions.train = predict(lasso.final.model, newx = x) 
predictions.test = predict(lasso.final.model, newx = x.test) 
CalculatePredictionStatistics("Lasso", predictions.train, predictions.test)

rm(iteration,lambdas,bestlambda,bestindex,predictions.test,predictions.train)  
rm(lasso.final.model)




##### Elastic Net  ####

Elastic_10fold <- cv.glmnet(x,y,alpha = 0.5 , nfolds = 10)
plot(Elastic_10fold, xlab = "gamma", main="Elastic")
rm(Elastic_10fold)

iteration=50
lambdas = NULL
for (i in 1:iteration)
{
  fit <- cv.glmnet(x,y,alpha = 0.5 , nfolds = 10)
  errors = data.frame(fit$lambda,fit$cvm, fit$lambda.1se)
  lambdas <- rbind(lambdas,errors)
}

# take mean cvm for each lambda
lambdas <- aggregate(lambdas[, 2:3], list(lambdas$fit.lambda), mean)

# select the best one
bestindex = which(lambdas[2]==min(lambdas[2]))
bestlambda = lambdas[bestindex,1]
# and now run glmnet once more with bestlambda


elastic.final.model <- glmnet(x,y,lambda=bestlambda, alpha = 0.5)

# calculate predictions
predictions.train = predict(elastic.final.model, newx = x) 
predictions.test = predict(elastic.final.model, newx = x.test) 
CalculatePredictionStatistics("Elastic", predictions.train, predictions.test)

# clean up variables
rm(iteration,lambdas,bestlambda,bestindex,predictions.test,predictions.train)  
rm(elastic.final.model)
rm(x,x.test)




##### PCR ####
library(pls)
#help(pcr)
set.seed(15)
student.model.pcr = pcr(G3~., data=student.train, validation = "CV", 
                        segments=10, segments.type="consecutive",scale=TRUE)
if (showInfo) summary(student.model.pcr)
if (showPlots) validationplot(student.model.pcr, val.type="MSEP")
# Calculate predictions for test set
ncomp = 16
predictions.train = predict(student.model.pcr, newdata=student.train, ncomp=ncomp)
predictions.test  = predict(student.model.pcr, newdata=student.test, ncomp=ncomp)
CalculatePredictionStatistics(paste("PCR", ncomp), predictions.train, predictions.test)

# clean up variables no longer needed
rm(student.model.pcr,ncomp)




##### PLS  #####
#library(pls)
#help(plsr)
set.seed(13)
student.model.pls = plsr(G3~., data=student.train, validation = "CV", 
                         segments=10, segments.type="consecutive")
if (showInfo) summary(student.model.pls)
if (showPlots) plot(student.model.pls,plottype="validation")
ncomp = 4
predictions.train = predict(student.model.pls, newdata=student.train, ncomp=ncomp)
predictions.test  = predict(student.model.pls, newdata=student.test, ncomp=ncomp)
CalculatePredictionStatistics(paste("PLS", ncomp), predictions.train, predictions.test)

# clean up variables no longer needed
rm(student.model.pls,ncomp)



###### DECISION TREES #####

##### Regression tree####
library(rpart)
#help(package=rpart)
#help(rpart)
#help(summary.rpart)
#help(rpart.control)
student.model.tree1 = rpart(G3~., data=student.train, control=rpart.control(minsplit=10))

par(mar=c(0.5, 0.5, 0.5, 0.5))
if (showInfo) summary(student.model.tree1)
if (showPlots) plot(student.model.tree1)
if (showPlots) text(student.model.tree1)
if (showPlots) plotcp(student.model.tree1, upper=c("splits"))
predictions.train = as.vector(predict(student.model.tree1, newdata=student.train, type="vector"))
predictions.test  = as.vector(predict(student.model.tree1, newdata=student.test,  type="vector"))
CalculatePredictionStatistics("tree 1", predictions.train, predictions.test)

#install.packages("rattle")
#install.packages("rpart.plot")
library(rattle)
# rattle()
library(rpart.plot)
library(RColorBrewer)
fancyRpartPlot(student.model.tree1)

printcp(student.model.tree1)

cp.optimal =  student.model.tree1$cptable[which.min(student.model.tree1$cptable[,"xerror"]),"CP"]

##### prune the tree 1 CD above minimal ####
#help(prune.rpart)
student.model.tree2 = prune(student.model.tree1, cp=cp.optimal)
if (showPlots) plot(student.model.tree2)
if (showPlots) text(student.model.tree2)
if (showPlots) plotcp(student.model.tree2)
if (showInfo) summary(student.model.tree2)
predictions.train = as.vector(predict(student.model.tree2, newdata=student.train, type="vector"))
predictions.test  = as.vector(predict(student.model.tree2, newdata=student.test,  type="vector"))
CalculatePredictionStatistics("tree 2", predictions.train, predictions.test)

fancyRpartPlot(student.model.tree2, uniform=TRUE, main="Pruned Classification Tree")


# clean up variables no longer needed
rm(student.model.tree1); rm(student.model.tree2); rm(cp.optimal)



##### Bagging 700 ####
library(ipred)
#help(bagging)
nbagg =  700
student.model.bagging = bagging(G3~., data=student.train, coob=TRUE, nbagg=nbagg)
if (showInfo)  student.model.bagging
#summary(student.model.bagging)
predictions.train = predict(student.model.bagging, newdata=student.train)
predictions.test  = predict(student.model.bagging, newdata=student.test)
CalculatePredictionStatistics("bagging", predictions.train, predictions.test)

# clean up variables no longer needed
rm(student.model.bagging); rm(nbagg)



##### Boosting #####
library(gbm)
#help(gbm)
n.trees = 7000
student.model.boosting = gbm(G3~., data=student.train, distribution="gaussian",cv.folds=10, bag.fraction=1, n.trees=n.trees, interaction.depth = 5)
gbm.perf(student.model.boosting, method="cv")
predictions.train = predict(student.model.boosting, newdata=student.train, n.trees=n.trees)
predictions.test  = predict(student.model.boosting, newdata=student.test,  n.trees=n.trees)
CalculatePredictionStatistics("boosting", predictions.train, predictions.test)

summary(student.model.boosting)
par(mfrow=c(1,2))
plot(student.model.boosting, i="absences")
plot(student.model.boosting, i="failures")
par(mfrow=c(1,1))


# clean up variables no longer needed
rm(student.model.boosting); rm(n.trees)



###### Random forest #####
library(randomForest)
#help(randomForest)
n.tree =  400
student.model.RandomForest = randomForest(G3~., data=student.train, ntree=n.tree)
student.model.RandomForest
if (showPlots) plot(student.model.RandomForest)
if (showInfo) round(importance(student.model.RandomForest), 2)
predictions.train <- predict(student.model.RandomForest, newdata=student.train, type="response")
predictions.test  = predict(student.model.RandomForest, newdata=student.test,  type="response")
CalculatePredictionStatistics("Random Forest.400", predictions.train, predictions.test)

importance(student.model.RandomForest)
varImpPlot(student.model.RandomForest)
plot(student.model.RandomForest)

if (saveMainResults) write.table(round(importance(student.model.RandomForest), 2), sep=";", file = "VarImportance.csv")

# clean up variables no longer needed
rm(student.model.RandomForest)

##############################################################################


#### ZINB ####
library(pscl)

student.model.zip.full <- zeroinfl(G3~ .|.,data = student.train, link = "logit", dist = "poisson")
summary(student.model.zip.full)

Anova(student.model.zip.full)
predictions.train = predict(student.model.zip.full, newdata=student.train, n.trees=n.trees)
predictions.test  = predict(student.model.zip.full, newdata=student.test,  n.trees=n.trees)
CalculatePredictionStatistics("ZeroInflatedPOisson.overfit", predictions.train, predictions.test)

#### ZINB 2 ####
student.model.zip <- zeroinfl(G3~ absences+failures+Mjob+health+Fjob+age+goout+schoolsup|
                                absences+failures+Mjob+health+Fjob+age+goout+schoolsup,
                              data = student.train, 
                              link = "logit", dist = "poisson")
summary(student.model.zip)

predictions.train = predict(student.model.zip, newdata=student.train, n.trees=n.trees)
predictions.test  = predict(student.model.zip, newdata=student.test,  n.trees=n.trees)
CalculatePredictionStatistics("ZeroInflatedPOisson.optimized", predictions.train, predictions.test)


#### Negative Binomial ####
library(car)
student.model.ZINB <- zeroinfl(G3~ .|.,data = student.train, link = "logit", dist = "negbin")
summary(student.model.ZINB)
predictions.train = predict(student.model.ZINB, newdata=student.train, n.trees=n.trees)
predictions.test  = predict(student.model.ZINB, newdata=student.test,  n.trees=n.trees)
CalculatePredictionStatistics("ZeroInflatedNegBinom", predictions.train, predictions.test)
Anova(student.model.ZINB)
#### Negative Binomial 2 ####

student.model.ZINB.optimized <- zeroinfl(G3~ absences+failures+Mjob+health+Fjob+age|
                                           absences+failures+Mjob+health+Fjob+age,
                                         data = student.train, 
                                         link = "logit", dist = "negbin")

summary(student.model.ZINB.optimized)
predictions.train = predict(student.model.ZINB.optimized, newdata=student.train, n.trees=n.trees)
predictions.test  = predict(student.model.ZINB.optimized, newdata=student.test,  n.trees=n.trees)
CalculatePredictionStatistics("ZeroInflatedNegBinom.optimized", predictions.train, predictions.test)

cbind(AIC(student.model.zip.full),AIC(student.model.zip),
      AIC(student.model.ZINB),AIC(student.model.ZINB.optimized))


#### hurdle ####

student.hurdle <-hurdle(G3~ absences+failures+Mjob+health+Fjob+age|
                    absences+failures+Mjob+health+Fjob+age,
                  data = student.train,
                  zero.dist="binomial",link="logit",dist="negbin")
summary(student.hurdle)
predictions.train = predict(student.hurdle, newdata=student.train, n.trees=n.trees)
predictions.test  = predict(student.hurdle, newdata=student.test,  n.trees=n.trees)
CalculatePredictionStatistics("student.hurdle", predictions.train, predictions.test)


##############################################################################

PredictionStatistics <- PredictionStatistics[ 
  order( PredictionStatistics[,1],PredictionStatistics[,3] ) , ]
PredictionStatistics
if (saveMainResults) write.table(PredictionStatistics,sep = ",",
                                 file = "PredictionStatistics.csv")

##############################################################################

###############################################################################
##################### BIG DATA IMPLEMENTATION ##################################
###############################################################################



####### h20.io random forest demo ####
cat("\014") # clear console

# Model 1 : Random forest H2O versions 
library(h2o) 
## Start a local cluster with 2GB RAM
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE, 
                    max_mem_size = '2g',min_mem_size='1g')

#   http://localhost:54321/    should be active in browser now. 
h2o.clusterInfo()
n.tree = 1000


student.train.h = as.h2o(student.train)
X.predictors = colnames(student.train)
X.predictors = X.predictors[-match("G3", X.predictors)]

student.h20 = h2o.randomForest(y="G3", x=X.predictors,
                                   training_frame = student.train.h,
                                   ntrees = n.tree)
predictions.train = as.data.frame(h2o.predict(student.h20, 
                                              newdata = as.h2o(student.train)))$predict
predictions.test = as.data.frame(h2o.predict(student.h20,
                                             newdata = as.h2o(student.test)))$predict
CalculatePredictionStatistics("Random Forest- h2o.io", predictions.train, predictions.test)

PredictionStatistics <- PredictionStatistics[ 
  order( PredictionStatistics[,1],PredictionStatistics[,3] ) , ]
PredictionStatistics
if (saveMainResults) write.table(PredictionStatistics,sep = ",",
                                 file = "PredictionStatistics.csv")
####### end of h20.io random forest demo ####