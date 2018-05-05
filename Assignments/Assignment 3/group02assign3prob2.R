
##########################
## CDS Assignment 3
## Group No: 2
## Problem No 2
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
library(tm)
library(wordcloud)
library(stringr)
library(caret)
library(e1071)

## Reading text file and making necessary changes
smsdata <- read.table("http://www.souravsengupta.com/cds2017/evaluation/smsdata.txt",
                      header = FALSE, sep = "\t", quote = "", stringsAsFactors=FALSE)
names(smsdata)<-c("type","message")
head(smsdata)
str(smsdata)
smsdata$type <- as.factor(smsdata$type)
table(smsdata$type)

###=================================================================================
###### Cleaning the Data and Visualizing
###=================================================================================

clean_data <- smsdata
str(clean_data)
clean_data$message <- tolower(smsdata$message)
clean_data$message <- removeNumbers(clean_data$message)
clean_data$message <- removeWords(clean_data$message, words = stopwords("en"))
clean_data$message <- stripWhitespace(clean_data$message)
clean_data$message <- removePunctuation(clean_data$message)

clean_data_good <- clean_data[clean_data$type == "good",]
clean_data_spam <- clean_data[clean_data$type == "spam",]

## Creating Word Cloud
wordcloud(clean_data_good$message, min.freq = 100)
wordcloud(clean_data_spam$message, min.freq = 60)

###=================================================================================
###### Creating Corpus and Document Term Matrix
###=================================================================================

sms_corpus <- Corpus(VectorSource(clean_data$message))   # Corpus of Messages

### Creating a Document Term Matrix
docMat <- DocumentTermMatrix(sms_corpus)                 # Creating Document Term Matrix

### Removing less frequent termss (less than 100)
frequentDocs <- findFreqTerms(x = docMat, lowfreq = 100)
docMat <- DocumentTermMatrix(sms_corpus, control = list(dictionary = frequentDocs))

### Converting Matrix to DataFrame
cleaned_df <- data.table(label = as.factor(smsdata$type),
                         message_len = nchar(smsdata$message),
                         num_caps = str_count(smsdata$message, "\\b[A-Z]{2,}\\b"),
                         num_upper = str_count(smsdata$message, "[A-Z]"),
                         num_number = str_count(smsdata$message, "[0-9]"),
                         as.matrix(docMat))
cleaned_df[,1:4]
cleaned_df[, mean(message_len), by = label]     # Average message length for each label
cleaned_df[, mean(num_caps), by = label]        # Average No. of CAPS for each label
cleaned_df[, mean(num_number), by = label]      # Average No. of numbers for each label


##==============================================================================
#### Creating Data Partition
##==============================================================================

set.seed(234)
inTrain <- createDataPartition(cleaned_df$label, p = 0.7, list = FALSE)
partTrain <- cleaned_df[inTrain]
partValid <- cleaned_df[-inTrain]


##==============================================================================
#### Training Models with Support Vector Machines
###=============================================================================

##################################################
#####            Linear Kernal             #######
##################################################

## On Repeated trial and error Cost = 0.1 proved to be best among (0.001,0.01,0.1,1,10)
svm_lin <- svm(label ~.,
               data = partTrain,
               kernel = "linear",
               cost = 1,
               gamma = 0.1,
               scale = FALSE)

summary(svm_lin)

## Prediction
pred_svmlin <- predict(svm_lin, newdata = partValid)

## Confusion Matrix
confusionMatrix(partValid$label, pred_svmlin)

## Accuracy : 0.9815
## Prediction good spam
##       good 1439    9
##       spam   22  202


##################################################
#####   Polynomial Kernal with degree = 2   ######
##################################################

svm_poly <- svm(label ~.,
                data = partTrain,
                kernel = "polynomial",
                coef0 = 1,
                degree = 2,
                cost = 1,
                gamma = 0.1,
                scale = FALSE)

summary(svm_poly)

## Prediction
pred_svmPoly <- predict(svm_poly, newdata = partValid)

## Confustion Matrix
confusionMatrix(partValid$label, pred_svmPoly)

## Accuracy : 0.9785
## Prediction good spam
##       good 1433   15
##       spam   21  203

## Note: Polynomial when reduced to degree 1 gave the best result
######## which is same as linear kernal result

##################################################
#####            Radial Kernal              ######
##################################################

svm_rad <- svm(label ~.,
               data = partTrain, 
               kernel = "radial",
               cost = 1, 
               gamma = 0.1, 
               scale = FALSE)

summary(svm_rad)

## Prediction
pred_svmRad <- predict(svm_rad, newdata = partValid)

## Confustion Matrix
confusionMatrix(partValid$label, pred_svmRad)

## Accuracy : 0.9677
## Prediction good spam
##       good 1441    7
##       spam   47  177


##################################################
#####            Sigmoid Kernal             ######
##################################################

svm_sig <- svm(label ~., 
               data = partTrain, 
               kernel = "sigmoid", 
               coef = 1,
               cost = 1, 
               gamma = 0.1, 
               scale = FALSE)

summary(svm_sig)

## Prediction
pred_svmSig <- predict(svm_sig, newdata = partValid)

## Confustion Matrix
confusionMatrix(partValid$label, pred_svmSig)

## Accuracy : 0.8654
## Prediction good spam
##       good 1447    1
##       spam  224    0

## All spam missclassified

##################################################
#####             Final Result             #######
##################################################

## Thus we see that Linear Kernal has the best result with Accuracy of 98.15%
confusionMatrix(partValid$label, pred_svmlin)
