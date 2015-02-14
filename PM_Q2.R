##Question 1
##Load the Alzheimer's disease data using the commands: 


##install.packages("AppliedPredictiveModeling")
##install.packages("caret")

library(AppliedPredictiveModeling)
library(caret)
data(AlzheimerDisease)

##Which of the following commands will create training and test sets with about 50% of the observations assigned to each? 


adData = data.frame(diagnosis,predictors)
str(adData)

##Question 2

##Load the cement data using the commands: library(AppliedPredictiveModeling)
data(concrete)
library(caret)
set.seed(975)
inTrain = createDataPartition(mixtures$CompressiveStrength, p = 3/4)[[1]])
training = mixtures[ inTrain,]
testing = mixtures[-inTrain,]

##Make a plot of the outcome (CompressiveStrength) versus the index of the samples.
##Color by each of the variables in the data set 
##(you may find the cut2() function in the Hmisc package useful for turning continuous covariates into factors). 
##What do you notice in these plots? 

##featurePlot(x=training, y = training$CompressiveStrength, plot ="pairs")
featurePlot(x=training, y = training$CompressiveStrength)
##There is a step-like pattern in the plot of outcome versus index in the training set that isn't explained by any of the predictor variables so there may be a variable missing.
library(hmisc)
idx<-1:nrow(training)
training$idx<-idx
featurePlot(x=training, y = training$CompressiveStrength)
qplot(x=idx, y = training$CompressiveStrength)
cutStr<-cut2(training$CompressiveStrength,g=4)

qplot(x=idx, y = training$CompressiveStrength, fill = cutStr)

##3
## Make a histogram and confirm the SuperPlasticizer variable is skewed. Normally you might use the log transform to try to make the data more symmetric. Why would that be a poor choice for this variable?
hist(log(training$Superplasticizer)+1)
hist(training$Superplasticizer)

##There are a large number of values that are the same and even if you took the log(SuperPlasticizer + 1) they would still all be identical so the distribution would not be symmetric.


## Question 4
## Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

## Find all the predictor variables in the training set that begin with IL. 
## Perform principal components on these variables with the preProcess() function from the caret package. Calculate the number of principal components needed to capture 80% of the variance. How many are there?
str(training)
trainingIL<-training[,grep("^IL_",colnames(training))]
str(trainingIL)
## do same for testing
testingIL<-testing[,grep("^IL_",colnames(testing))]
str(trainingIL)

prep<-preProcess(trainingIL, method= "pca",thresh=.8)
##prep<-preProcess(trainingIL, method= "pca")
prep$numComp
##7

##Q5

##Create a training data set consisting of only the predictors with variable names beginning with IL and the diagnosis. 
##Build two predictive models, one using the predictors as they are and one using PCA with principal components explaining 80% of the variance in the predictors. Use method="glm" in the train function. 
##What is the accuracy of each method in the test set? Which is more accurate?

## Load the Alzheimer's disease data using the commands:
library(caret)
library(AppliedPredictiveModeling)
set.seed(3433)
data(AlzheimerDisease)
adData = data.frame(diagnosis,predictors)
inTrain = createDataPartition(adData$diagnosis, p = 3/4)[[1]]
training = adData[ inTrain,]
testing = adData[-inTrain,]

##add diagnosis column 
trainingIL$diagnosis<-training$diagnosis
testingIL$diagnosis<-testing$diagnosis
str(testingIL)
d<-ncol(trainingIL)

##No PCA
modelfit<-train(diagnosis~., data=trainingIL, method='glm')
modelfit
modelfit$finalModel
predictions<-predict(modelfit, newdata = testingIL)
predictions
confusionMatrix(predictions,testingIL$diagnosis)
##Accuracy : 0.6463


## build to models w/ pca
prep80<-preProcess(trainingIL[,-d], method= "pca",thresh=.8)
##build pca dataset
train80<-predict(prep80, trainingIL[-13])
##train model
model80<-train(trainingIL$diagnosis~.,method = "glm", data = train80)

##build pca test set
test80<-predict(prep80, testingIL[-13])
confusionMatrix(testing$diagnosis, predict(model80,test80))
