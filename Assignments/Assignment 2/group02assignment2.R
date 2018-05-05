
library(corrplot)
library(caTools)
library(car)

wineTrain <- read.csv("http://www.souravsengupta.com/cds2017/evaluation/wineTrain.csv") 
str(wineTrain)
head(wineTrain)
summary(wineTrain)

# Splitting the data into training and testing in 70:30 ratio
set.seed(23)
tr.split = sample.split(wineTrain$Quality, SplitRatio = 0.7)
train.data = subset(wineTrain, tr.split==TRUE)
test.data = subset(wineTrain, tr.split==FALSE)

# Checking the trend and correlation between the variables
pairs(train.data)
cor(train.data)
corrplot.mixed(cor(train.data))
# Here we see that Alcohol is the most correlated variable (wrt Quality) and FixedAcidity is highly correlated with CitricAcid, Density & pH.

# Creating Base Model with all variables:
linFit1 <- lm(Quality ~. , data = train.data)
summary(linFit1)
# Check for heteroskedasticity
ncvTest(linFit1)
# Test for multicollinearity
vif(linFit1) # FixedAcidity & Density has high vif

# Updating model by removing varibales with high vif
linFit2 <- update(linFit1, ~. -(FixedAcidity+Density))
summary(linFit2)
vif(linFit2)

# Updating model by removing insignificant variables:
linFit3 <- update(linFit2, ~. -(CitricAcid+ResidualSugar))
summary(linFit3)
vif(linFit3)

cor(train.data$FreeSulphurDioxide, train.data$TotalSulphurDioxide)
# Correlation between FreeSulphurDioxide and TotalSulphurDioxide is 0.67 thus keeping only TotalSulphurDioxide which is more significant. Model with remaining variables:
linFit4 <- lm(Quality ~ VolatileAcidity + Chlorides + TotalSulphurDioxide + pH + Sulphates + Alcohol, data = train.data)
summary(linFit4)
vif(linFit4)

# Adjusted R2 again decreased so keeping the earlier linFit3 model and inserting a new interacting term as Sulphate^2
linFit5 <- lm(Quality ~ VolatileAcidity + Chlorides + TotalSulphurDioxide + pH + Sulphates + Alcohol + I(Sulphates^2), data = train.data)
summary(linFit5)
vif(linFit5) # vif of Sulphate & Sulphate^2 is high but it was intentionally added so we keep this.
ncvTest(linFit5)

# Adding one more interaction term Alcohol*Sulphate^2
linFit6 <- lm(Quality ~ VolatileAcidity + Chlorides + TotalSulphurDioxide + pH + Sulphates + Alcohol + I(Sulphates^2) +I(Alcohol*Sulphates^2), data = train.data)
summary(linFit6)
vif(linFit6)


# linFit6 has higher R2 but vif is too high due to too much interaction.
# Thus we keep linFit5 as the best model.
# Checking this with ANOVA to check the best model
anova(linFit1, linFit2, linFit3, linFit4, linFit5, linFit6)
# This confirms that the best model is linFit5 and we will use this as final model

# Calculating R2 and RMSE for the train data
RSS.t <- sum((linFit5$residuals)^2)
TSS.t <- sum((mean(train.data$Quality) - train.data$Quality)^2)
R2.t <- 1 - RSS.t/TSS.t
R2.t
RMSE.t <- sqrt(R2.t/nrow(train.data))

# Prediction on Test Data:
pred <- predict(linFit5, newdata = test.data)
RSS.p <- sum((pred - test.data$Quality)^2)
TSS.p <- sum((mean(test.data$Quality) - test.data$Quality)^2)
R2.p <- 1 - RSS.p/TSS.p
print(paste("Predicted R-Squared of Test set is: ",R2.p), quote = FALSE)
RMSE.p <- sqrt(RSS.p/nrow(test.data))
RMSE.p

# Here we see that test prediction R2 (0.411) is more that training R2 (0.349).
# So we now check if this is true for different sample.
# Using Cross Validation we will now compare the R2 over train and test data for 20
nTrials = 20
cr.data <- matrix(data = NA, nrow = nTrials, ncol = 5)
for(i in 1:nTrials){
  set.seed(i)
  split = sample.split(wineTrain$Quality, SplitRatio = 0.7)
  cr.train.data = subset(wineTrain, split==TRUE)
  cr.test.data = subset(wineTrain, split==FALSE)
  
  crModel <- lm(formula(linFit5), data = cr.train.data)
  
  cr.RSS.t <- sum((crModel$residuals)^2)
  cr.TSS.t <- sum((mean(cr.train.data$Quality) - cr.train.data$Quality)^2)
  cr.data[i,1] <- 1 - cr.RSS.t/cr.TSS.t
  cr.data[i,2] <- sqrt(cr.RSS.t/nrow(cr.train.data))
  
  cr.pred <- predict(crModel, newdata = cr.test.data)
  cr.RSS.p <- sum((cr.pred - cr.test.data$Quality)^2)
  cr.TSS.p <- sum((mean(cr.test.data$Quality) - cr.test.data$Quality)^2)
  cr.data[i,3] <- 1 - cr.RSS.p/cr.TSS.p
  cr.data[i,4] <- sqrt(cr.data[i,3]/nrow(cr.test.data))
  
}

cr.data[,5] <- cr.data[,3] - cr.data[,1]
cr.data <- data.frame(cr.data)
colnames(cr.data) <- c("Train_R2", "Train_RMSE", "Test_R2", "Test_RMSE", "Diff_R2")
cr.data
summary(cr.data)
# Thus we see that average train R2 and test R2 are nearly same which means that our model is not over or under predicting.

# Ploting the final selected model for further modification
plot(linFit5)

# Now that we have fixed our model, We will try to improve it
# Remove outliers and high-leverage points
cd <- cooks.distance(linFit5)
train.clean <- train.data[abs(cd) < 3*mean(cd), ] # outliers are those values whose cook distance is greater than 3 times mean of cd
nrow(train.clean)

# Fit the best model to the clean data
linFit7 <- lm(formula(linFit5), data = train.clean)
summary(linFit7)
# plot(linFit7)

# Prediction on Test Data:
pred.c <- predict(linFit7, newdata = test.data)
RSS.c <- sum((pred.c - test.data$Quality)^2)
TSS.c <- sum((mean(test.data$Quality) - test.data$Quality)^2)
R2.c <- 1 - RSS.c/TSS.c
print(paste("Predicted R-Squared of Test set is: ",R2.c), quote = FALSE)
RMSE.c <- sqrt(RSS.c/nrow(test.data))
RMSE.c

# Thus here we are choosing the final model with cleaned data with summary as below:
summary(linFit7)
# Here final trainig R2 is 0.4268 and predicted R2 is 0.4219

