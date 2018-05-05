
##########################
## CDS Assignment 3
## Group No: 2
## Problem No 3
##########################

## Probelm Solving Approach:
#### Text Processing is same as Problem 1
#### Model is trained with SVM using 4 kernals: 'linear', 'polynomial', 'radial' & 'sigmoid'
#### Kernal Parameters were tuned manually to get the best ouput
#### We checked the accuracy for each Kernal and we got the highest accuracy with linear (98.39%)

## Main Text processing Package Used : 'tm' package

## Online Reference for text processing: 
#### "http://simranmetric.com/classifying-spam-sms-messages-using-naive-bayes-classifier/"

### Analysis of the Results is done at the end of all the models

######################################################

## Clearing library
rm(list = ls())

## Loading Required Libraries
library(data.table)
library(caret)
library(e1071)
library(Rtsne)
library(doParallel)

#==============================================================================
### Reading Data
#==============================================================================

datapath <- "C:/Users/shubh/Google Drive/Study/PGDBA/IIMC Shared Drive/Datasets/Kaggle/Digit Recognizer/"

digitTrain <- fread(input = paste0(datapath, "train.csv"))
digitTest <- fread(input = paste0(datapath, "test.csv"))

digitTrain[,.N,by=label]

digitComb <- rbindlist(list(digitTrain[,-1],digitTest)) ## Combining train & test

##=============================================================================
# Using T-sne for Dimension Reduction
##=============================================================================

set.seed(23)
digitTsneComb <- Rtsne(digitComb, dims = 2, perplexity = 30, verbose=TRUE, max_iter = 1000)

## Seperating train and test data after Dimension Reduction
tsneComb <- digitTsneComb$Y
tsneTrain <- data.table(as.factor(digitTrain$label), tsneComb[1:nrow(digitTrain),]) 
tsneTest <- data.table(tsneComb[-c(1:nrow(digitTrain)),]) 
colnames(tsneTrain) <- c("label", "Vec1","Vec2")
colnames(tsneTest) <- c("Vec1","Vec2")

# Visualizing
colors = rainbow(length(unique(digitTrain$label)))
names(colors) = unique(digitTrain$label)
plot(tsneTrain[,-"label"], t='n', main="tsne")
text(tsneTrain[,-"label"], labels = tsneTrain$label, col = colors[tsneTrain$label])
## We can see that the clusters are very nicely seperated

##=====================================================================================
### Performing SVM on the Reduced Dimension Data
##=====================================================================================

## Patitioning data in Train & Validation set
set.seed(234)
inTrain <- createDataPartition(tsneTrain$label, p = 0.7, list = FALSE)
trainSplit <- tsneTrain[c(inTrain)]
trainSplit[,.N]
ValidSplit <- tsneTrain[-c(inTrain)]
ValidSplit[,.N]

svmModel <- svm(label ~., data = trainSplit,
                kernel = "radial",
                gamma = 0.01,
                cost = 10,
                scale = FALSE)

summary(svmModel)
# Predicting the model results on validation set
pred <- predict(svmModel, ValidSplit)
confusionMatrix(ValidSplit$label, pred)


svmModel1 <- svm(label ~., data = trainSplit,
                kernel = "radial",
                gamma = 1,
                cost = 0.5,
                scale = FALSE)

summary(svmModel1)
# Predicting the model results on validation set
pred <- predict(svmModel1, ValidSplit)
confusionMatrix(ValidSplit$label, pred)

## Trial 2
svmModel2 <- svm(label ~., data = trainSplit,
                 kernel = "radial",
                 gamma = 0.1,
                 cost = 0.01,
                 scale = FALSE)

summary(svmModel2)
# Predicting the model results on validation set
pred <- predict(svmModel2, ValidSplit)
confusionMatrix(ValidSplit$label, pred)


#=====================================================================
## Not run till now
svmTune <-  tune(svm, 
                 label ~ ., 
                 data = trainSplit, 
                 kernel = "radial", # exp( - gamma * (u - v) . (u - v) )
                 ranges = list(gamma = c(0.001, 0.01, 0.1, 1, 10),
                               cost = c(0.01, 0.05, 0.1, 0.5, 1)), 
                 scale = FALSE)

summary(svmTune)                     # summary of tuning parameters
bestFit <- tuneModel$best.model      # extract the best tuned model
summary(bestFit)                     # best parameter-tuned model
##=====================================================================

##=========================================================================
### Running the best model on full data
##=========================================================================

svmModel <- svm(label ~., data = tsneTrain,
                kernel = "radial",
                gamma = 1,
                cost = 0.5,
                scale = FALSE)

summary(svmModel)

# Predicting on the Test data
testPred <- predict(svmModel, tsneTest)

## Creating file for Kaggle Submission
submission <- data.table(ImageID = seq(1:28000), Label = testPred)
write.csv(x = submission, file = "submission1.csv", row.names = FALSE)








