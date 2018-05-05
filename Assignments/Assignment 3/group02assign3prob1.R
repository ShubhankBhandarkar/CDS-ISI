
##########################
## CDS Assignment 3
## Group No: 2
## Problem No 1
##########################

## Probelm Solving Approach:
#### Firstly we read the data and checked the type and structure of the data
#### Then we used text mining to clean the data and visualize the most frequent words.
#### We then separated the words thorugh Document Term Matrix to get frequency of each word
#### We added some more variables such as message length & Number of caps and performed basic EDA.
#### On Data preparation, we applied tree using 'tree' & 'rpart', grown full tree and then pruned it. 
#### We then applied bagging and random forest using random forest package.
#### Finally we applied a gradient boosting using XGBoost package.
#### We checked the accuracy at each step and we got the highest accuracy for random forest (98.39%)

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
library(tree)
library(randomForest)

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
#### Training Models with Trees
###==============================================================================

######################################################
####   'Tree' with 4 most important variables    #####
######################################################

## Message length, number of caps, num of Numbers & word 'call' seemed to be important 
## from the initial EDA. So creating basic tree with those variables.

library(tree)
treeFit <- tree(label ~ message_len + num_caps + call + num_number, data = partTrain)
summary(treeFit)

## Prediction on Test
pred_treeFit <- predict(treeFit, newdata = partValid, type = "class")

## Confusion Matrix
confusionMatrix(partValid$label, pred_treeFit)

## Accuracy : 0.9713
## Prediction good spam
##       good 1434   14
##       spam   34  190


######################################################
####       rPart Tree with all variables         #####
######################################################

library(rpart)
rpartFit <- rpart(label ~., data = partTrain)
summary(rpartFit)

## Prediction on Test
pred_rpartFit <- predict(rpartFit, newdata = partValid, type = "class")

## Confusion Matrix
confusionMatrix(partValid$label, pred_rpartFit)

## Accuracy : 0.9761
## Prediction good spam
##       good 1430   18
##       spam   22  202


######################################################
####       Large Tree with all variables         #####
######################################################

ctrl <- tree.control(nobs = nrow(partTrain), # number of sample points
                     mincut = 1,             # minimum points in each child
                     minsize = 2,            # minimum points in each parent
                     mindev = 0)             # minimum information gain to split

tree_large <- tree(label ~ ., data = data.frame(partTrain),
                   split = "deviance",
                   method = "recursive.partition",
                   control = ctrl)

plot(tree_large)

## Prediction on Train
pred_train_ltree <- predict(tree_large,
                            newdata = data.frame(partTrain),
                            type = "class")

## Train Confusion Matrix
confusionMatrix(partTrain$label, pred_train_ltree)

## Prediction on Validation Set
pred_valid_ltree <- predict(tree_large,
                            newdata = data.frame(partValid),
                            type = "class")
## Train Confusion Matrix
confusionMatrix(partValid$label, pred_valid_ltree)

## Accuracy : 0.9707
## Prediction good spam
##       good 1422   26
##       spam   23  201


######################################################
####         Pruned Classification Tree          #####
######################################################

cvTree <- cv.tree(tree_large, FUN = prune.misclass, K = 20) # K-fold Cross-Validation
cbind(cvTree$size, cvTree$dev, cvTree$k)                    # check cvTree output
plot(cvTree$size, cvTree$dev, type="b")                     # plot deviance vs size
plot(cvTree$k, cvTree$dev, type="b")                        # plot deviance vs alpha

ptreeFit <- prune.misclass(tree_large, best = 3)            # prune tree to best size
plot(ptreeFit)
text(ptreeFit, pretty = FALSE)

## Prediction on Train
pred_train_ptree <- predict(ptreeFit,
                            newdata = data.frame(partTrain),
                            type = "class")

## Train Confusion Matrix
confusionMatrix(partTrain$label, pred_train_ptree)

## Prediction on Validation Set
pred_valid_ptree <- predict(ptreeFit,
                            newdata = data.frame(partValid),
                            type = "class")
## Train Confusion Matrix
confusionMatrix(partValid$label, pred_valid_ptree)

## Accuracy : 0.9761
## Prediction good spam
##       good 1430   18
##       spam   22  202


######################################################
####        Bagging with Random Forest           #####
######################################################

set.seed(234)
bagFit <- randomForest(label ~., data = data.frame(partTrain),
                       mtry = 56,                      # mtry = number of variables
                       ntree = 500,
                       importance = TRUE)
summary(bagFit)

## Importance
importance(bagFit)
varImpPlot(bagFit)

## Prediction on Test
pred_bag <- predict(bagFit, newdata = data.frame(partValid), type = "class")

## Confusion Matrix
confusionMatrix(partValid$label, pred_bag)

## Accuracy : 0.9797
## Prediction good spam
##       good 1431   17
##       spam   17  207


######################################################
####               Random Forest                 #####
######################################################

set.seed(234)
rfFit <- randomForest(label ~., data = data.frame(partTrain),
                      mtry = 10,          # mtry = best selected '10' among (5,10,15,25)
                      ntree = 500,
                      importance = TRUE)
summary(rfFit)

## Importance
importance(rfFit)
varImpPlot(rfFit)

## Prediction on Test
pred_rfFit <- predict(rfFit, newdata = data.frame(partValid), type = "class")

## Confusion Matrix
confusionMatrix(partValid$label, pred_rfFit)

## Accuracy : 0.9839
## Prediction good spam
##       good 1445    3
##       spam   29  195


######################################################
####              Gradiant Boosting              #####
######################################################

library(xgboost)

dtrain <- xgb.DMatrix(as.matrix(partTrain[,-1]), label = as.numeric(partTrain$label)-1)
dtest <-  xgb.DMatrix(as.matrix(partValid[,-1]), label = as.numeric(partValid$label)-1)

params <- list(booster = "gbtree",
               objective = "binary:logistic",
               eta = 0.07,
               gamma = 1,
               max_depth = 9,
               min_child_weight = 1,
               subsample = 0.5,
               colsample_bytree = 0.3,
               eval_metric = "logloss")

## Applying Cross-Validation to get the best model
xgbcv <- xgb.cv(params = params,
                data = dtrain,
                nrounds = 1000,
                nfold = 10, 
                showsd = T, 
                stratified = T, 
                print_every_n = 10, 
                early_stopping_rounds = 10,
                maximize = F, 
                nthread = 4)

## Choosing the best model
xgb1 <- xgb.train(params = params, 
                  data = dtrain, 
                  nrounds = xgbcv$best_iteration,
                  watchlist = list(val = dtest,train = dtrain), 
                  print_every_n = 10,
                  early_stopping_rounds = 10, 
                  maximize = F)


# Model prediction on Train
xgb.pred <- predict(xgb1, newdata = dtrain, type = "class")
xgb.pred <- ifelse(xgb.pred >= 0.5,1,0)

## Confusion Matrix
confusionMatrix(as.numeric(partTrain$label)-1, xgb.pred)

# Model prediction on Test
xgb.pred <- predict(xgb1, newdata = dtest, type = "class")
xgb.pred <- ifelse(xgb.pred >= 0.5,1,0)

## Confusion Matrix
confusionMatrix(as.numeric(partValid$label)-1, xgb.pred)

## Accuracy : 0.9833
## Prediction good spam
##       good 1440    8
##       spam   20  204

###########################################
#### Final Comments
###########################################

## We can see that top 4 variables classifies nearly 97% of the data
## Also, acccuracy have increased with each consecutive model.
## Lowest accurecy with basic tree with only 4 most important variables.
## rpart and pruned tree increased the accuracy to some extent
## Bagging helped us to gain some more accuracy
## Random Forest is the winner here with highest accuracy
## XGBoost too matches up to random forest with slightly less accuracy

############################################

