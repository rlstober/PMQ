##Question 1
##Load the cell segmentation data from the AppliedPredictiveModeling package using the commands:
library(AppliedPredictiveModeling)
data(segmentationOriginal)
library(caret)

##The ambiguity in the first question of Quiz 3 was a pain point in a past session of this course that I took, so allow me to save you some time by clarifying:

# Partition into training/testing is already done in the CASE variable of segmentationOriginal, so you do not have to use createDataPartition().
# The variable you are trying to predict is CLASS (never actually mentioned in the question).
# I hope that this saves you some time and frustration.

str(segmentationOriginal)
head(segmentationOriginal)
# 
# 1. Subset the data to a training set and testing set based on the Case variable in the data set. 
# 2. Set the seed to 125 and fit a CART model with the rpart method using all predictor variables and default caret settings. 
set.seed(125)
inTrain<-segmentationOriginal$Case =="Train"
inTest<-segmentationOriginal$Case =="Test"
training = segmentationOriginal[ inTrain,]
testing = segmentationOriginal[inTest,]
modfit<-train(Class~., method="rpart",data=training)
print(modfit$finalModel)
plot(modfit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modfit$finalModel, use.n=T, all = T, cex=.8)


library(rattle)
fancyRpartPlot(modfit$finalModel)

# 1) root 1009 373 PS (0.63032706 0.36967294)  
# 2) TotalIntenCh2< 45323.5 454  34 PS (0.92511013 0.07488987) *
#   3) TotalIntenCh2>=45323.5 555 216 WS (0.38918919 0.61081081)  
# 6) FiberWidthCh1< 9.673245 154  47 PS (0.69480519 0.30519481) *
#   7) FiberWidthCh1>=9.673245 401 109 WS (0.27182045 0.72817955) *
#   

# a. TotalIntench2 = 23,000; FiberWidthCh1 = 10; PerimStatusCh1=2 
##PS
# b. TotalIntench2 = 50,000; FiberWidthCh1 = 10;VarIntenCh4 = 100 
##WS
# c. TotalIntench2 = 57,000; FiberWidthCh1 = 8;VarIntenCh4 = 100 
##PS
# d. FiberWidthCh1 = 8;VarIntenCh4 = 100; PerimStatusCh1=2 
## not possible


# a. PS 
# b. WS 
# c. PS
# d. Not possible to predict 


##Question 2
# If K is small in a K-fold cross validation is the bias in the estimate of out-of-sample (test set) accuracy smaller or bigger? If K is small is the variance in the estimate of out-of-sample (test set) accuracy smaller or bigger. Is K large or small in leave one out cross validation?

#The bias is larger and the variance is smaller. Under leave one out cross validation K is equal to the sample size.

# Question 3
# Load the olive oil data using the commands:
library(carat)
  
library(pgmm)
data(olive)
olive = olive[,-1]
str(olive)
# (NOTE: If you have trouble installing the pgmm package, you can download the olive dataset here: olive_data.zip. After unzipping the archive, you can load the file using the load() function in R.)
# These data contain information on 572 different Italian olive oils from multiple regions in Italy. 
#Fit a classification tree where Area is the outcome variable. 
#Then predict the value of area for the following data frame using the tree command with all defaults
 
inTrain = createDataPartition(olive$Area, p = 3/4)[[1]]
training = olive[ inTrain,]
testing = olive[-inTrain,]
modfit<-train(Area~., method="rpart",data=training)
print(modfit$finalModel)
plot(modfit$finalModel, uniform = TRUE, main = "Classification Tree")
text(modfit$finalModel, use.n=T, all = T, cex=.8)
fancyRpartPlot(modfit$finalModel)

newdata = as.data.frame(t(colMeans(olive)))
predict(modfit,newdata)

What is the resulting prediction? Is the resulting prediction strange? Why or why not?
# ] 2.809917
#2.875. It is strange because Area should be a qualitative variable - but tree is reporting the average value of Area as a numeric variable in the leaf predicted for newdata



# Question 4
# Load the South Africa Heart Disease Data and create training and test sets with the following code:

library(ElemStatLearn)
data(SAheart)
set.seed(8484)
train = sample(1:dim(SAheart)[1],size=dim(SAheart)[1]/2,replace=F)
trainSA = SAheart[train,]
testSA = SAheart[-train,]

str(trainSA)

#Then set the seed to 13234 
set.seed(8484)
#and fit a logistic regression model (method="glm", be sure to specify family="binomial") 
#with Coronary Heart Disease (chd) as the outcome and 
#age at onset, current alcohol consumption, obesity levels, cumulative tabacco, type-A behavior, 
#and low density lipoprotein cholesterol as predictors. 
modfit<-train(chd~alcohol+obesity+tobacco+typea+ldl, method="glm", family="binomial", data=trainSA)
predTest<-predict(modfit,testSA)
predTrain<-predict(modfit,trainSA)

#Calculate the misclassification rate for your model using this function 
#and a prediction on the "response" scale:
  
missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}

#What is the misclassification rate on the training set? 
missClass(trainSA$chd, predTrain)
#0.2813853

#What is the misclassification rate on the test set?
missClass(testSA$chd, predTest)
#test: 0.3116883

#Test Set Misclassification: 0.31 
#Training Set: 0.27

# Question 5
# Load the vowel.train and vowel.test data sets:
  library(ElemStatLearn)
data(vowel.train)
data(vowel.test) 
# Set the variable y to be a factor variable in both the training and test set. 
str(vowel.train)
vowel.train$y<-as.factor(vowel.train$y)
vowel.test$y<-as.factor(vowel.test$y)

#Then set the seed to 33833. 
set.seed(33833)

#Fit a random forest predictor relating the factor variable y to the remaining variables. 

#Read about variable importance in random forests here: 
#http://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#ooberr 
#The caret package uses by defualt the Gini importance. 
#Calculate the variable importance using the varImp function in the caret package. 
#What is the order of variable importance?
modfit<-train(y~.,method ="rf", data =vowel.train, importance=TRUE, prox=TRUE)
modfit<-train(y~.,method ="rf", data =vowel.train)
varImp(modfit)

# rf variable importance
# 
# Overall
# x.1  100.000
# x.2   99.399
# x.5   45.034
# x.6   25.357
# x.8   22.150
# x.4   12.237
# x.9    8.300
# x.3    6.973
# x.7    4.147
# x.10   0.000


#The order of the variables is:
#.2, x.1, x.5, x.6, x.8, x.4, x.9, x.3, x.7,x.10
